# Wildfire Data Simulation
# Generate synthetic data based on specified proportions for each site

library(dplyr)

set.seed(123)  # For reproducible results

# Define sites and their sample sizes
## The old code for later use (commented out for backup):
# Original 5 sites:
# sites <- c("Felton", "Paradise", "Redwood Estates", "Saratoga", "Tahoe-Donner")
# sample_sizes <- c(200, 300, 150, 250, 180)

# Previous 25 cities version:
# sites <- c("Felton", "Paradise", "Redwood Estates", "Saratoga", "Tahoe-Donner", 
#            "Santa Rosa", "Napa", "Grass Valley", "Auburn", "Calistoga",
#            "San Francisco", "Los Angeles", "San Diego", "Sacramento", "Fresno",
#            "Oakland", "Bakersfield", "Anaheim", "Riverside", "Stockton",
#            "San Jose", "Long Beach", "Modesto", "Chula Vista", "Fremont")
# sample_sizes <- c(200, 300, 150, 250, 180, 220, 190, 160, 210, 140,  # First 10
#                   250, 300, 280, 220, 200, 240, 180, 210, 190, 170,  # Next 10  
#                   230, 160, 140, 120, 110)  # Last 5 (total 25)

# Current: 10 sites only
sites <- c("Felton", "Paradise", "Redwood Estates", "Saratoga", "Tahoe-Donner", 
           "Santa Rosa", "Napa", "Grass Valley", "Auburn", "Calistoga")
sample_sizes <- c(200, 300, 150, 250, 180, 220, 190, 160, 210, 140)

# Function to generate data for each site
generate_site_data <- function(site_name, n) {
  
  # Year built - random years between 1950-2020
  year_built <- sample(1950:2020, n, replace = TRUE)
  
  # Site name
  site <- rep(site_name, n)
  
  # Roof status based on site-specific proportions
  ## Old code with original percentages (kept for backup):
  # roof_probs <- switch(site_name,
  #   # Original 10 cities:
  #   "Felton" = c(0.96, 0.04),  # Needs Maintenance 96%, Good 4%
  #   "Paradise" = c(0.50, 0.50),  # Needs Maintenance 50%, Good 50%
  #   "Redwood Estates" = c(0.99, 0.01),  # Needs Maintenance 99%, Good 1%
  #   "Saratoga" = c(0.92, 0.08),  # Needs Maintenance 92%, Good 8%
  #   "Tahoe-Donner" = c(0.22, 0.78),  # Needs Maintenance 22%, Good 78%
  #   "Santa Rosa" = c(0.75, 0.25),  # Needs Maintenance 75%, Good 25%
  #   "Napa" = c(0.65, 0.35),  # Needs Maintenance 65%, Good 35%
  #   "Grass Valley" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15%
  #   "Auburn" = c(0.70, 0.30),  # Needs Maintenance 70%, Good 30%
  #   "Calistoga" = c(0.80, 0.20),  # Needs Maintenance 80%, Good 20%
  #   # Additional 15 cities:
  #   "San Francisco" = c(0.60, 0.40),  # Needs Maintenance 60%, Good 40%
  #   "Los Angeles" = c(0.70, 0.30),  # Needs Maintenance 70%, Good 30%
  #   "San Diego" = c(0.45, 0.55),  # Needs Maintenance 45%, Good 55%
  #   "Sacramento" = c(0.75, 0.25),  # Needs Maintenance 75%, Good 25%
  #   "Fresno" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15%
  #   "Oakland" = c(0.65, 0.35),  # Needs Maintenance 65%, Good 35%
  #   "Bakersfield" = c(0.80, 0.20),  # Needs Maintenance 80%, Good 20%
  #   "Anaheim" = c(0.55, 0.45),  # Needs Maintenance 55%, Good 45%
  #   "Riverside" = c(0.72, 0.28),  # Needs Maintenance 72%, Good 28%
  #   "Stockton" = c(0.78, 0.22),  # Needs Maintenance 78%, Good 22%
  #   "San Jose" = c(0.58, 0.42),  # Needs Maintenance 58%, Good 42%
  #   "Long Beach" = c(0.68, 0.32),  # Needs Maintenance 68%, Good 32%
  #   "Modesto" = c(0.82, 0.18),  # Needs Maintenance 82%, Good 18%
  #   "Chula Vista" = c(0.50, 0.50),  # Needs Maintenance 50%, Good 50%
  #   "Fremont" = c(0.62, 0.38)  # Needs Maintenance 62%, Good 38%
  # )
  
  ## Previous 25 cities version (commented out for backup):
  # roof_probs <- switch(site_name,
  #   # Original 10 cities (adjusted):
  #   "Felton" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15% (was 96%/4%)
  #   "Paradise" = c(0.50, 0.50),  # Needs Maintenance 50%, Good 50%
  #   "Redwood Estates" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15% (was 99%/1%)
  #   "Saratoga" = c(0.80, 0.20),  # Needs Maintenance 80%, Good 20% (was 92%/8%)
  #   "Tahoe-Donner" = c(0.25, 0.75),  # Needs Maintenance 25%, Good 75% (was 22%/78%)
  #   "Santa Rosa" = c(0.75, 0.25),  # Needs Maintenance 75%, Good 25%
  #   "Napa" = c(0.65, 0.35),  # Needs Maintenance 65%, Good 35%
  #   "Grass Valley" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15%
  #   "Auburn" = c(0.70, 0.30),  # Needs Maintenance 70%, Good 30%
  #   "Calistoga" = c(0.80, 0.20),  # Needs Maintenance 80%, Good 20%
  #   # Additional 15 cities (adjusted):
  #   "San Francisco" = c(0.60, 0.40),  # Needs Maintenance 60%, Good 40%
  #   "Los Angeles" = c(0.70, 0.30),  # Needs Maintenance 70%, Good 30%
  #   "San Diego" = c(0.45, 0.55),  # Needs Maintenance 45%, Good 55%
  #   "Sacramento" = c(0.75, 0.25),  # Needs Maintenance 75%, Good 25%
  #   "Fresno" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15%
  #   "Oakland" = c(0.65, 0.35),  # Needs Maintenance 65%, Good 35%
  #   "Bakersfield" = c(0.80, 0.20),  # Needs Maintenance 80%, Good 20%
  #   "Anaheim" = c(0.55, 0.45),  # Needs Maintenance 55%, Good 45%
  #   "Riverside" = c(0.72, 0.28),  # Needs Maintenance 72%, Good 28%
  #   "Stockton" = c(0.78, 0.22),  # Needs Maintenance 78%, Good 22%
  #   "San Jose" = c(0.58, 0.42),  # Needs Maintenance 58%, Good 42%
  #   "Long Beach" = c(0.68, 0.32),  # Needs Maintenance 68%, Good 32%
  #   "Modesto" = c(0.82, 0.18),  # Needs Maintenance 82%, Good 18%
  #   "Chula Vista" = c(0.50, 0.50),  # Needs Maintenance 50%, Good 50%
  #   "Fremont" = c(0.62, 0.38)  # Needs Maintenance 62%, Good 38%
  # )
  
  ## 15%-85% adjusted version (commented out for backup):
  # roof_probs <- switch(site_name,
  #   "Felton" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15% (was 96%/4%)
  #   "Paradise" = c(0.50, 0.50),  # Needs Maintenance 50%, Good 50%
  #   "Redwood Estates" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15% (was 99%/1%)
  #   "Saratoga" = c(0.80, 0.20),  # Needs Maintenance 80%, Good 20% (was 92%/8%)
  #   "Tahoe-Donner" = c(0.25, 0.75),  # Needs Maintenance 25%, Good 75% (was 22%/78%)
  #   "Santa Rosa" = c(0.75, 0.25),  # Needs Maintenance 75%, Good 25%
  #   "Napa" = c(0.65, 0.35),  # Needs Maintenance 65%, Good 35%
  #   "Grass Valley" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15%
  #   "Auburn" = c(0.70, 0.30),  # Needs Maintenance 70%, Good 30%
  #   "Calistoga" = c(0.80, 0.20)  # Needs Maintenance 80%, Good 20%
  # )
  
  # Current: 10 sites with original percentages
  roof_probs <- switch(site_name,
    "Felton" = c(0.96, 0.04),  # Needs Maintenance 96%, Good 4%
    "Paradise" = c(0.50, 0.50),  # Needs Maintenance 50%, Good 50%
    "Redwood Estates" = c(0.99, 0.01),  # Needs Maintenance 99%, Good 1%
    "Saratoga" = c(0.92, 0.08),  # Needs Maintenance 92%, Good 8%
    "Tahoe-Donner" = c(0.22, 0.78),  # Needs Maintenance 22%, Good 78%
    "Santa Rosa" = c(0.75, 0.25),  # Needs Maintenance 75%, Good 25%
    "Napa" = c(0.65, 0.35),  # Needs Maintenance 65%, Good 35%
    "Grass Valley" = c(0.85, 0.15),  # Needs Maintenance 85%, Good 15%
    "Auburn" = c(0.70, 0.30),  # Needs Maintenance 70%, Good 30%
    "Calistoga" = c(0.80, 0.20)  # Needs Maintenance 80%, Good 20%
  )
  
  ## 50% balanced version (commented out):
  # roof_probs <- c(0.50, 0.50)  # Needs Maintenance 50%, Good 50% for all cities
  roof_status <- sample(c("needs maintenance", "good"), n, replace = TRUE, prob = roof_probs)
  
  # VENT_HAZARD_STATUS based on site-specific proportions
  ## Old code with original percentages (kept for backup):
  # vent_probs <- switch(site_name,
  #   # Original 10 cities:
  #   "Felton" = c(0.652, 0.348),  # HIGH_RISK 65.2%, LOW_RISK 34.8%
  #   "Paradise" = c(0.367, 0.633),  # HIGH_RISK 36.7%, LOW_RISK 63.3%
  #   "Redwood Estates" = c(0.773, 0.227),  # HIGH_RISK 77.3%, LOW_RISK 22.7%
  #   "Saratoga" = c(0.680, 0.320),  # HIGH_RISK 68.0%, LOW_RISK 32.0%
  #   "Tahoe-Donner" = c(0.140, 0.860),  # HIGH_RISK 14.0%, LOW_RISK 86.0%
  #   "Santa Rosa" = c(0.550, 0.450),  # HIGH_RISK 55.0%, LOW_RISK 45.0%
  #   "Napa" = c(0.480, 0.520),  # HIGH_RISK 48.0%, LOW_RISK 52.0%
  #   "Grass Valley" = c(0.720, 0.280),  # HIGH_RISK 72.0%, LOW_RISK 28.0%
  #   "Auburn" = c(0.420, 0.580),  # HIGH_RISK 42.0%, LOW_RISK 58.0%
  #   "Calistoga" = c(0.630, 0.370),  # HIGH_RISK 63.0%, LOW_RISK 37.0%
  #   # Additional 15 cities:
  #   "San Francisco" = c(0.35, 0.65),  # HIGH_RISK 35%, LOW_RISK 65%
  #   "Los Angeles" = c(0.60, 0.40),  # HIGH_RISK 60%, LOW_RISK 40%
  #   "San Diego" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Sacramento" = c(0.55, 0.45),  # HIGH_RISK 55%, LOW_RISK 45%
  #   "Fresno" = c(0.70, 0.30),  # HIGH_RISK 70%, LOW_RISK 30%
  #   "Oakland" = c(0.40, 0.60),  # HIGH_RISK 40%, LOW_RISK 60%
  #   "Bakersfield" = c(0.75, 0.25),  # HIGH_RISK 75%, LOW_RISK 25%
  #   "Anaheim" = c(0.30, 0.70),  # HIGH_RISK 30%, LOW_RISK 70%
  #   "Riverside" = c(0.65, 0.35),  # HIGH_RISK 65%, LOW_RISK 35%
  #   "Stockton" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "San Jose" = c(0.38, 0.62),  # HIGH_RISK 38%, LOW_RISK 62%
  #   "Long Beach" = c(0.52, 0.48),  # HIGH_RISK 52%, LOW_RISK 48%
  #   "Modesto" = c(0.68, 0.32),  # HIGH_RISK 68%, LOW_RISK 32%
  #   "Chula Vista" = c(0.28, 0.72),  # HIGH_RISK 28%, LOW_RISK 72%
  #   "Fremont" = c(0.42, 0.58)  # HIGH_RISK 42%, LOW_RISK 58%
  # )
  
  ## Previous 25 cities version (commented out for backup):
  # vent_probs <- switch(site_name,
  #   # Original 10 cities (adjusted):
  #   "Felton" = c(0.652, 0.348),  # HIGH_RISK 65.2%, LOW_RISK 34.8%
  #   "Paradise" = c(0.367, 0.633),  # HIGH_RISK 36.7%, LOW_RISK 63.3%
  #   "Redwood Estates" = c(0.773, 0.227),  # HIGH_RISK 77.3%, LOW_RISK 22.7% (was 77.3%/22.7%)
  #   "Saratoga" = c(0.680, 0.320),  # HIGH_RISK 68.0%, LOW_RISK 32.0%
  #   "Tahoe-Donner" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 14%/86%)
  #   "Santa Rosa" = c(0.550, 0.450),  # HIGH_RISK 55.0%, LOW_RISK 45.0%
  #   "Napa" = c(0.480, 0.520),  # HIGH_RISK 48.0%, LOW_RISK 52.0%
  #   "Grass Valley" = c(0.720, 0.280),  # HIGH_RISK 72.0%, LOW_RISK 28.0%
  #   "Auburn" = c(0.420, 0.580),  # HIGH_RISK 42.0%, LOW_RISK 58.0%
  #   "Calistoga" = c(0.630, 0.370),  # HIGH_RISK 63.0%, LOW_RISK 37.0%
  #   # Additional 15 cities (adjusted):
  #   "San Francisco" = c(0.35, 0.65),  # HIGH_RISK 35%, LOW_RISK 65%
  #   "Los Angeles" = c(0.60, 0.40),  # HIGH_RISK 60%, LOW_RISK 40%
  #   "San Diego" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Sacramento" = c(0.55, 0.45),  # HIGH_RISK 55%, LOW_RISK 45%
  #   "Fresno" = c(0.70, 0.30),  # HIGH_RISK 70%, LOW_RISK 30%
  #   "Oakland" = c(0.40, 0.60),  # HIGH_RISK 40%, LOW_RISK 60%
  #   "Bakersfield" = c(0.75, 0.25),  # HIGH_RISK 75%, LOW_RISK 25%
  #   "Anaheim" = c(0.30, 0.70),  # HIGH_RISK 30%, LOW_RISK 70%
  #   "Riverside" = c(0.65, 0.35),  # HIGH_RISK 65%, LOW_RISK 35%
  #   "Stockton" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "San Jose" = c(0.38, 0.62),  # HIGH_RISK 38%, LOW_RISK 62%
  #   "Long Beach" = c(0.52, 0.48),  # HIGH_RISK 52%, LOW_RISK 48%
  #   "Modesto" = c(0.68, 0.32),  # HIGH_RISK 68%, LOW_RISK 32%
  #   "Chula Vista" = c(0.28, 0.72),  # HIGH_RISK 28%, LOW_RISK 72%
  #   "Fremont" = c(0.42, 0.58)  # HIGH_RISK 42%, LOW_RISK 58%
  # )
  
  ## 15%-85% adjusted version (commented out for backup):
  # vent_probs <- switch(site_name,
  #   "Felton" = c(0.652, 0.348),  # HIGH_RISK 65.2%, LOW_RISK 34.8%
  #   "Paradise" = c(0.367, 0.633),  # HIGH_RISK 36.7%, LOW_RISK 63.3%
  #   "Redwood Estates" = c(0.773, 0.227),  # HIGH_RISK 77.3%, LOW_RISK 22.7%
  #   "Saratoga" = c(0.680, 0.320),  # HIGH_RISK 68.0%, LOW_RISK 32.0%
  #   "Tahoe-Donner" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 14%/86%)
  #   "Santa Rosa" = c(0.550, 0.450),  # HIGH_RISK 55.0%, LOW_RISK 45.0%
  #   "Napa" = c(0.480, 0.520),  # HIGH_RISK 48.0%, LOW_RISK 52.0%
  #   "Grass Valley" = c(0.720, 0.280),  # HIGH_RISK 72.0%, LOW_RISK 28.0%
  #   "Auburn" = c(0.420, 0.580),  # HIGH_RISK 42.0%, LOW_RISK 58.0%
  #   "Calistoga" = c(0.630, 0.370)  # HIGH_RISK 63.0%, LOW_RISK 37.0%
  # )
  
  # Current: 10 sites with original percentages
  vent_probs <- switch(site_name,
    "Felton" = c(0.652, 0.348),  # HIGH_RISK 65.2%, LOW_RISK 34.8%
    "Paradise" = c(0.367, 0.633),  # HIGH_RISK 36.7%, LOW_RISK 63.3%
    "Redwood Estates" = c(0.773, 0.227),  # HIGH_RISK 77.3%, LOW_RISK 22.7%
    "Saratoga" = c(0.680, 0.320),  # HIGH_RISK 68.0%, LOW_RISK 32.0%
    "Tahoe-Donner" = c(0.140, 0.860),  # HIGH_RISK 14.0%, LOW_RISK 86.0%
    "Santa Rosa" = c(0.550, 0.450),  # HIGH_RISK 55.0%, LOW_RISK 45.0%
    "Napa" = c(0.480, 0.520),  # HIGH_RISK 48.0%, LOW_RISK 52.0%
    "Grass Valley" = c(0.720, 0.280),  # HIGH_RISK 72.0%, LOW_RISK 28.0%
    "Auburn" = c(0.420, 0.580),  # HIGH_RISK 42.0%, LOW_RISK 58.0%
    "Calistoga" = c(0.630, 0.370)  # HIGH_RISK 63.0%, LOW_RISK 37.0%
  )
  
  ## 50% balanced version (commented out):
  # vent_probs <- c(0.50, 0.50)  # HIGH_RISK 50%, LOW_RISK 50% for all cities
  vent_hazard_status <- sample(c("HIGH_RISK", "LOW_RISK"), n, replace = TRUE, prob = vent_probs)
  
  # DECK_HAZARD_STATUS based on site-specific proportions
  ## Old code with original percentages (kept for backup):
  # deck_probs <- switch(site_name,
  #   # Original 10 cities:
  #   "Felton" = c(0.36, 0.64),  # HIGH_RISK 36%, LOW_RISK 64%
  #   "Paradise" = c(0.18, 0.82),  # HIGH_RISK 18%, LOW_RISK 82%
  #   "Redwood Estates" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "Saratoga" = c(0.08, 0.92),  # HIGH_RISK 8%, LOW_RISK 92%
  #   "Tahoe-Donner" = c(0.08, 0.92),  # HIGH_RISK 8%, LOW_RISK 92%
  #   "Santa Rosa" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Napa" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85%
  #   "Grass Valley" = c(0.40, 0.60),  # HIGH_RISK 40%, LOW_RISK 60%
  #   "Auburn" = c(0.12, 0.88),  # HIGH_RISK 12%, LOW_RISK 88%
  #   "Calistoga" = c(0.30, 0.70),  # HIGH_RISK 30%, LOW_RISK 70%
  #   # Additional 15 cities:
  #   "San Francisco" = c(0.20, 0.80),  # HIGH_RISK 20%, LOW_RISK 80%
  #   "Los Angeles" = c(0.35, 0.65),  # HIGH_RISK 35%, LOW_RISK 65%
  #   "San Diego" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85%
  #   "Sacramento" = c(0.28, 0.72),  # HIGH_RISK 28%, LOW_RISK 72%
  #   "Fresno" = c(0.45, 0.55),  # HIGH_RISK 45%, LOW_RISK 55%
  #   "Oakland" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Bakersfield" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "Anaheim" = c(0.18, 0.82),  # HIGH_RISK 18%, LOW_RISK 82%
  #   "Riverside" = c(0.38, 0.62),  # HIGH_RISK 38%, LOW_RISK 62%
  #   "Stockton" = c(0.32, 0.68),  # HIGH_RISK 32%, LOW_RISK 68%
  #   "San Jose" = c(0.22, 0.78),  # HIGH_RISK 22%, LOW_RISK 78%
  #   "Long Beach" = c(0.28, 0.72),  # HIGH_RISK 28%, LOW_RISK 72%
  #   "Modesto" = c(0.42, 0.58),  # HIGH_RISK 42%, LOW_RISK 58%
  #   "Chula Vista" = c(0.16, 0.84),  # HIGH_RISK 16%, LOW_RISK 84%
  #   "Fremont" = c(0.24, 0.76)  # HIGH_RISK 24%, LOW_RISK 76%
  # )
  
  ## Previous 25 cities version (commented out for backup):
  # deck_probs <- switch(site_name,
  #   # Original 10 cities (adjusted):
  #   "Felton" = c(0.36, 0.64),  # HIGH_RISK 36%, LOW_RISK 64%
  #   "Paradise" = c(0.18, 0.82),  # HIGH_RISK 18%, LOW_RISK 82%
  #   "Redwood Estates" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "Saratoga" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 8%/92%)
  #   "Tahoe-Donner" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 8%/92%)
  #   "Santa Rosa" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Napa" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85%
  #   "Grass Valley" = c(0.40, 0.60),  # HIGH_RISK 40%, LOW_RISK 60%
  #   "Auburn" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 12%/88%)
  #   "Calistoga" = c(0.30, 0.70),  # HIGH_RISK 30%, LOW_RISK 70%
  #   # Additional 15 cities (adjusted):
  #   "San Francisco" = c(0.20, 0.80),  # HIGH_RISK 20%, LOW_RISK 80%
  #   "Los Angeles" = c(0.35, 0.65),  # HIGH_RISK 35%, LOW_RISK 65%
  #   "San Diego" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85%
  #   "Sacramento" = c(0.28, 0.72),  # HIGH_RISK 28%, LOW_RISK 72%
  #   "Fresno" = c(0.45, 0.55),  # HIGH_RISK 45%, LOW_RISK 55%
  #   "Oakland" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Bakersfield" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "Anaheim" = c(0.18, 0.82),  # HIGH_RISK 18%, LOW_RISK 82%
  #   "Riverside" = c(0.38, 0.62),  # HIGH_RISK 38%, LOW_RISK 62%
  #   "Stockton" = c(0.32, 0.68),  # HIGH_RISK 32%, LOW_RISK 68%
  #   "San Jose" = c(0.22, 0.78),  # HIGH_RISK 22%, LOW_RISK 78%
  #   "Long Beach" = c(0.28, 0.72),  # HIGH_RISK 28%, LOW_RISK 72%
  #   "Modesto" = c(0.42, 0.58),  # HIGH_RISK 42%, LOW_RISK 58%
  #   "Chula Vista" = c(0.16, 0.84),  # HIGH_RISK 16%, LOW_RISK 84%
  #   "Fremont" = c(0.24, 0.76)  # HIGH_RISK 24%, LOW_RISK 76%
  # )
  
  ## 15%-85% adjusted version (commented out for backup):
  # deck_probs <- switch(site_name,
  #   "Felton" = c(0.36, 0.64),  # HIGH_RISK 36%, LOW_RISK 64%
  #   "Paradise" = c(0.18, 0.82),  # HIGH_RISK 18%, LOW_RISK 82%
  #   "Redwood Estates" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
  #   "Saratoga" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 8%/92%)
  #   "Tahoe-Donner" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 8%/92%)
  #   "Santa Rosa" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
  #   "Napa" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85%
  #   "Grass Valley" = c(0.40, 0.60),  # HIGH_RISK 40%, LOW_RISK 60%
  #   "Auburn" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85% (was 12%/88%)
  #   "Calistoga" = c(0.30, 0.70)  # HIGH_RISK 30%, LOW_RISK 70%
  # )
  
  # Current: 10 sites with original percentages
  deck_probs <- switch(site_name,
    "Felton" = c(0.36, 0.64),  # HIGH_RISK 36%, LOW_RISK 64%
    "Paradise" = c(0.18, 0.82),  # HIGH_RISK 18%, LOW_RISK 82%
    "Redwood Estates" = c(0.50, 0.50),  # HIGH_RISK 50%, LOW_RISK 50%
    "Saratoga" = c(0.08, 0.92),  # HIGH_RISK 8%, LOW_RISK 92%
    "Tahoe-Donner" = c(0.08, 0.92),  # HIGH_RISK 8%, LOW_RISK 92%
    "Santa Rosa" = c(0.25, 0.75),  # HIGH_RISK 25%, LOW_RISK 75%
    "Napa" = c(0.15, 0.85),  # HIGH_RISK 15%, LOW_RISK 85%
    "Grass Valley" = c(0.40, 0.60),  # HIGH_RISK 40%, LOW_RISK 60%
    "Auburn" = c(0.12, 0.88),  # HIGH_RISK 12%, LOW_RISK 88%
    "Calistoga" = c(0.30, 0.70)  # HIGH_RISK 30%, LOW_RISK 70%
  )
  
  ## 50% balanced version (commented out):
  # deck_probs <- c(0.50, 0.50)  # HIGH_RISK 50%, LOW_RISK 50% for all cities
  deck_hazard_status <- sample(c("HIGH_RISK", "LOW_RISK"), n, replace = TRUE, prob = deck_probs)
  
  # Create data frame
  data.frame(
    Year.built = year_built,
    Site = site,
    Roof.status = roof_status,
    VENT_HAZARD_STATUS = vent_hazard_status,
    DECK_HAZARD_STATUS = deck_hazard_status,
    stringsAsFactors = FALSE
  )
}

# Generate data for all sites
simulated_data <- do.call(rbind, mapply(generate_site_data, sites, sample_sizes, SIMPLIFY = FALSE))

# Display summary statistics
cat("=== Simulated Data Summary ===\n")
cat("Total Sample Size:", nrow(simulated_data), "\n\n")

# Check proportions for each site
for (site in sites) {
  cat("=== ", site, " ===\n")
  site_data <- simulated_data[simulated_data$Site == site, ]
  
  cat("Sample Size:", nrow(site_data), "\n")
  
  # Roof status proportions
  roof_prop <- round(prop.table(table(site_data$Roof.status)) * 100, 1)
  cat("Roof Status:\n")
  print(roof_prop)
  
  # Vent hazard proportions
  vent_prop <- round(prop.table(table(site_data$VENT_HAZARD_STATUS)) * 100, 1)
  cat("Vent Hazard Status:\n")
  print(vent_prop)
  
  # Deck hazard proportions
  deck_prop <- round(prop.table(table(site_data$DECK_HAZARD_STATUS)) * 100, 1)
  cat("Deck Hazard Status:\n")
  print(deck_prop)
  
  cat("\n")
}

# Display first few rows
cat("=== Data Preview ===\n")
print(head(simulated_data, 10))

# Rename to final variable name
data <- simulated_data

# Save to CSV file
write.csv(data, "simulated_wildfire_data.csv", row.names = FALSE)
cat("\nData saved to 'simulated_wildfire_data.csv'\n")
