# sAP Globalization Quality dataset
# Guillaume Barbalat
# 17/04/2022

# Load data and packages
# read_clean.R produces final_matrix data.frame
# Lines 18-28 in read_clean.R are relevant

# Clear environment
rm(list = ls())

# Set working directory (replace with your actual path)
setwd("~/your_working_directory")

# Load libraries
library(dplyr)
library(lmtp)
library(table1)
library(ggplot2)
library(tidyr)

# Parameters
sensit <- ""  # Sensitivity label
data_file <- paste0("results_lmtp_Quality", sensit, ".RData")

# Load data
load(file = data_file)

# Index constants
idx_psi1 <- 1
idx_psi2 <- 2
idx_psi3 <- 3
idx_psi4 <- 4
idx_psi_null_KOFGI <- 5
idx_psi_mtp_KOFGI <- 6
idx_results_MSM_categ <- 7
idx_results_mtp_KOFGI <- 8
idx_matrix <- 9


# Helper functions --------------------------------------------------------

# Function: Create descriptive statistics table
print_table1 <- function(data) {
  my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits = 2, round.integers = FALSE),
         c("", "Mean (SD) \n Range" = sprintf("%s (&plusmn %s) %s-%s", MEAN, SD, MIN, MAX)))
  }

  glob_table <- table1(
    ~ Y + Y_0 + unemploy + SDI + urban + CSA + p90p100 + haqi + Quality
    | categ_KOFGI_labels,
    render.continuous = my.render.cont,
    data = data
  )
  return(glob_table)
}


# Function: Extract coefficients from model results
coeff_pullout <- function(coeff_multiple) {
  tmp <- do.call(c, coeff_multiple)
  all_idx <- do.call(cbind, tmp)
  data.frame(
    algo = rownames(all_idx),
    mean = rowMeans(all_idx)
  )
}

# Main Analysis Loop --------------------------------------------------------

# Loop through each dataset in psi_results_data
for (i in 1:length(psi_results_data)) {
  if (i == 10) {
    next
  }  # Skip all_GI index

  # Extract data components
  psi_1 <- psi_results_data[[i]][[idx_psi1]]
  psi_2 <- psi_results_data[[i]][[idx_psi2]]
  psi_3 <- psi_results_data[[i]][[idx_psi3]]
  psi_4 <- psi_results_data[[i]][[idx_psi4]]

  psi_null_KOFGI <- psi_results_data[[i]][[idx_psi_null_KOFGI]]
  psi_mtp_KOFGI <- psi_results_data[[i]][[idx_psi_mtp_KOFGI]]

  results_MSM_categ <- psi_results_data[[i]][[idx_results_MSM_categ]]
  results_MSM_categ <- lmtp_contrast(psi_2, psi_3, psi_4,
    ref = psi_1
  )

  results_mtp_KOFGI <- psi_results_data[[i]][[idx_results_mtp_KOFGI]]

  final_matrix_quality_2019 <- psi_results_data[[i]][[idx_matrix]]

  # Analysis for each dataset
  cat("### Analysis for cause: ", unique(final_matrix_quality_2019$cause), "\n")

  # Descriptive statistics
  cat("#### Descriptive Statistics Table:\n")
  print(print_table1(final_matrix_quality_2019))

  # Outlier analysis
  cat("#### Outlier Analysis (DALYs):\n")
  outliers_max <- final_matrix_quality_2019$log_Y >= quantile(final_matrix_quality_2019$log_Y, 0.99)
  cat("##### Locations with Highest DALYs:\n")
  print(final_matrix_quality_2019$location[outliers_max])
  print(final_matrix_quality_2019[outliers_max, ])

  outliers_min <- final_matrix_quality_2019$log_Y <= quantile(final_matrix_quality_2019$log_Y, 0.01)
  cat("##### Locations with Lowest DALYs:\n")
  print(final_matrix_quality_2019$location[outliers_min])
  print(final_matrix_quality_2019[outliers_min, ])

  # KOFGI category sizes
  cat("#### KOFGI Category Sizes:\n")
  print(levels(final_matrix_quality_2019$categ_KOFGI_labels))
  cat("##### Category sizes:\n")
  print(nrow(final_matrix_quality_2019[with(final_matrix_quality_2019, categ_KOFGI == 1), "KOFGI"]))
  print(nrow(final_matrix_quality_2019[with(final_matrix_quality_2019, categ_KOFGI == 2), "KOFGI"]))
  print(nrow(final_matrix_quality_2019[with(final_matrix_quality_2019, categ_KOFGI == 3), "KOFGI"]))
  print(nrow(final_matrix_quality_2019[with(final_matrix_quality_2019, categ_KOFGI == 4), "KOFGI"]))

  # Data visualization
  cat("#### Data Visualization:\n")
  g1 <- ggplot(final_matrix_quality_2019,
    aes(y = Y, x = as.factor(categ_KOFGI_labels), colour = as.factor(categ_KOFGI_labels))
  ) +
    geom_boxplot() +
    labs(x = "Globalization Index (2018)", y = "2019 DALYs", title = unique(final_matrix_quality_2019$cause)) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white", colour = "black"),
      strip.background = element_rect(colour = "black", fill = "white"),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(colour = "black", size = 12),
      plot.title = element_text(colour = "black", face = "bold", size = 20)
    )
  print(g1)

  g2 <- ggplot(final_matrix_quality_2019,
    aes(y = Y_0, x = as.factor(categ_KOFGI_labels), colour = as.factor(categ_KOFGI_labels))
  ) +
    geom_boxplot() +
    labs(x = "Globalization Index (2018)", y = "1990 DALYs", title = unique(final_matrix_quality_2019$cause)) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white", colour = "black"),
      strip.background = element_rect(colour = "black", fill = "white"),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(colour = "black", size = 12),
      plot.title = element_text(colour = "black", face = "bold", size = 20)
    )
  print(g2)

  g3 <- ggplot(final_matrix_quality_2019,
    aes(y = Y, x = KOFGI)
  ) +
    geom_point() +
    geom_smooth() +
    labs(x = "Globalization Index (2018)", y = "2019 DALYs", title = unique(final_matrix_quality_2019$cause)) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white", colour = "black"),
      strip.background = element_rect(colour = "black", fill = "white"),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(colour = "black", size = 12),
      plot.title = element_text(colour = "black", face = "bold", size = 20)
    )
  print(g3)

  # MSM category results
  cat("#### MSM Category Results:\n")
  print(results_MSM_categ)

  # Weight analysis
  cat("#### Weight Analysis (MSM):\n")
  weights <- vector(length = nrow(final_matrix_quality_2019))
  weights[psi_1$density_ratios != 0] <- psi_1$density_ratios[psi_1$density_ratios != 0]
  weights[psi_2$density_ratios != 0] <- psi_2$density_ratios[psi_2$density_ratios != 0]
  weights[psi_3$density_ratios != 0] <- psi_3$density_ratios[psi_3$density_ratios != 0]
  weights[psi_4$density_ratios != 0] <- psi_4$density_ratios[psi_4$density_ratios != 0]
  hist(weights, main = "Histogram of weights (Trimming weights > 0.990)", xlab = "Weight Value")
  print(summary(weights))

  #High weight observations
  if (sensit == "") {
    high_weights <- which(weights > 10)
    cat("##### Observations with Weights > 10:\n")
    print(final_matrix_quality_2019[high_weights, ])
  }
}
