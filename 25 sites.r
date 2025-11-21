# Simulate Roof.status for: logit p_ij = (β0 + b0_j) + (β1 + b1_j) * x_ij,
# with (b0_j, b1_j) ~ N(0, Sigma), Sigma = [[σ0^2, ρσ0σ1],[ρσ0σ1, σ1^2]]
# Use existing design by passing design=your_data, x_variable="Year.built", site_variable="Site".
# If design is NULL, it will generate one.
#
## Example Usage:
## # Use defaults (25 sites with 200 observations each = 5,000 total)
## sim1 <- simulate_roof(beta0 = -0.3, beta1 = 0.1, sd0 = 0.6, sd1 = 0.1, rho = -0.2)
##
## # Custom sample sizes (must be length 25)
## sim2 <- simulate_roof(
##   beta0 = -0.3, beta1 = 0.1, sd0 = 0.6, sd1 = 0.1, rho = -0.2,
##   n_per_site = rep(150, 25)  # 150 observations per site
## )
##
## # Varied sample sizes
## sim3 <- simulate_roof(
##   beta0 = -0.3, beta1 = 0.1, sd0 = 0.6, sd1 = 0.1, rho = -0.2,
##   n_per_site = sample(100:300, 25, replace = TRUE)  # Random sizes 100-300
## )
##
## # Full customization (different sample sizes and year ranges per site)
## sim4 <- simulate_roof(
##   beta0 = -0.3, beta1 = 0.1, sd0 = 0.6, sd1 = 0.1, rho = -0.2,
##   n_per_site = sample(100:300, 25, replace = TRUE),  # Random sizes per site
##   year_min = rep(1924, 25),  # Site-specific min years (all same for simplicity)
##   year_max = rep(2024, 25)   # Site-specific max years (all same for simplicity)
## )

simulate_roof <- function(beta0, beta1, sd0, sd1, rho,
                          design = NULL,
                          x_var = "Year.built",
                          site_var = "Site",
                          # if generating a design:
                          n_per_site = rep(200, 25),  # observations per site (length 25)
                          year_min = rep(1924, 25),     # minimum year for each site (length 25)
                          year_max = rep(2024, 25),     # maximum year for each site (length 25)
                          seed = NULL,
                          keep_random_effects = TRUE,
                          add_prob = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(is.numeric(beta0), is.numeric(beta1),
            is.numeric(sd0), sd0 >= 0,
            is.numeric(sd1), sd1 >= 0,
            is.numeric(rho), rho >= -1, rho <= 1,
            is.numeric(n_per_site), length(n_per_site) == 25, all(n_per_site > 0),
            is.numeric(year_min), length(year_min) == 25,
            is.numeric(year_max), length(year_max) == 25,
            all(year_min <= year_max))

  # 1) Build or validate design ---------------------------------------------
  if (is.null(design)) {
    # Use fixed site names (25 California locations)
    site_names <- c("Felton", "Paradise", "Redwood Estates", "Saratoga", "Tahoe-Donner",
                     "Santa Rosa", "Napa", "Petaluma", "Sebastopol", "Healdsburg",
                     "Calistoga", "St. Helena", "Yountville", "Sonoma", "Glen Ellen",
                     "Forestville", "Guerneville", "Monte Rio", "Duncan Mills", "Occidental",
                     "Bodega Bay", "Jenner", "Sea Ranch", "Gualala", "Point Arena")
    n_sites <- 25

    # Generate sites and corresponding years with site-specific ranges，year built is discrete uniform distribution
    site_data <- mapply(function(site, n, ymin, ymax) {
      list(sites = rep(site, n),
           years = sample(ymin:ymax, size = n, replace = TRUE))
    }, site_names, n_per_site, year_min, year_max, SIMPLIFY = FALSE)
    
    sites <- unlist(lapply(site_data, function(x) x$sites))
    x_raw <- unlist(lapply(site_data, function(x) x$years))
    design <- data.frame(setNames(list(sites), site_var),
                         setNames(list(x_raw), x_var),
                         check.names = FALSE)
  } else {
    stopifnot(all(c(x_var, site_var) %in% names(design)))
  }

  # coerce site to factor (stable order)
  design[[site_var]] <- factor(design[[site_var]])
  site_levels <- levels(design[[site_var]])
  J <- length(site_levels)

  # x used in the linear predictor (scaled/standardized)
  x_raw <- as.numeric(design[[x_var]])
  x <- as.numeric(scale(x_raw))

  # 2) Draw random effects per site ------------------------------------------
  if (sd0 == 0 && sd1 == 0) {
    # No random effects (fixed intercept and slope)
    b0 <- rep(0, J)
    b1 <- rep(0, J)
    RE <- cbind(b0 = b0, b1 = b1)
    rownames(RE) <- site_levels
    Sigma <- matrix(c(0, 0, 0, 0), nrow = 2, byrow = TRUE)
  } else if (sd0 == 0) {
    # Random slope only (fixed intercept)
    b0 <- rep(0, J)
    b1 <- rnorm(J, mean = 0, sd = sd1)
    RE <- cbind(b0 = b0, b1 = b1)
    rownames(RE) <- site_levels
    Sigma <- matrix(c(0, 0, 0, sd1^2), nrow = 2, byrow = TRUE)
  } else if (sd1 == 0) {
    # Random intercept only (fixed slope)
    b0 <- rnorm(J, mean = 0, sd = sd0)
    b1 <- rep(0, J)
    RE <- cbind(b0 = b0, b1 = b1)
    rownames(RE) <- site_levels
    Sigma <- matrix(c(sd0^2, 0, 0, 0), nrow = 2, byrow = TRUE)
  } else {
    # Full random effects with correlation
    Sigma <- matrix(c(sd0^2, rho*sd0*sd1,
                      rho*sd0*sd1, sd1^2), nrow = 2, byrow = TRUE)
    
    # draw (b0, b1) for each site using Cholesky
    Z <- matrix(rnorm(J * 2), nrow = J, ncol = 2)
    # chol returns upper-tri U with t(U)%*%U = Sigma; use Z %*% t(U) to get cov=Sigma
    U <- chol(Sigma)
    RE <- Z %*% t(U)
    colnames(RE) <- c("b0", "b1")
    rownames(RE) <- site_levels
  }

  # 3) Map REs to rows and generate outcome ----------------------------------
  idx <- as.integer(design[[site_var]])
  b0j <- RE[idx, 1]
  b1j <- RE[idx, 2]
  eta <- (beta0 + b0j) + (beta1 + b1j) * x
  p   <- 1 / (1 + exp(-eta))
  y   <- rbinom(n = length(p), size = 1, prob = p)

  # 4) Return -----------------------------------------------------------------
  out <- design
  out$Roof.status <- y
  if (add_prob) out$.p <- p

  res <- list(
    data   = out,
    Sigma  = Sigma,
    params = list(beta0 = beta0, beta1 = beta1, sd0 = sd0, sd1 = sd1, rho = rho)
  )
  if (keep_random_effects) {
    res$random_effects <- data.frame(Site = site_levels,
                                     b0 = RE[,1], b1 = RE[,2],
                                     row.names = NULL)
  }
  return(res)
}
