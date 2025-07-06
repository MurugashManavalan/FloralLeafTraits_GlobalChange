# ---- Function: Install and Load Required Packages ----

install_and_load_all_packages <- function() {
  cran_packages <- c(
    "readr", "lme4", "lmerTest", "ggplot2", "vegan", "dplyr", "corrplot", 
    "igraph", "qgraph", "Hmisc", "ggrepel", "tidyr", "plyr", "ggeffects", 
    "car", "reshape2", "factoextra", "purrr", "here", "devtools", "brms"
  )
  
  missing_cran <- cran_packages[!cran_packages %in% installed.packages()[, "Package"]]
  if (length(missing_cran) > 0) {
    install.packages(missing_cran)
  } else {
    message("✅ All CRAN packages are already installed.")
  }
  
  if (!"PCAtest" %in% installed.packages()[, "Package"]) {
    if (!"devtools" %in% installed.packages()[, "Package"]) {
      install.packages("devtools")
    }
    devtools::install_github("arleyc/PCAtest")
  } else {
    message("✅ PCAtest is already installed.")
  }
  
  if (!"ggbiplot" %in% installed.packages()[, "Package"]) {
    if (!"devtools" %in% installed.packages()[, "Package"]) {
      install.packages("devtools")
    }
    devtools::install_github("vqv/ggbiplot")
  } else {
    message("✅ ggbiplot is already installed.")
  }
  
  all_packages <- c(cran_packages, "PCAtest", "ggbiplot")
  invisible(lapply(all_packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
  
  message("🎉 All packages installed and loaded successfully.")
}


# ---- Function: Log and Square Root Transform Response Variables ----

log_sqrt_transform <- function(df, cols) {
  if (is.numeric(cols)) {
    cols <- names(df)[cols]
  }
  
  for (col in cols) {
    log_col_name <- paste0("log_", col)
    sqrt_col_name <- paste0("sqrt_", col)
    df[[log_col_name]] <- log(df[[col]])
    df[[sqrt_col_name]] <- sqrt(df[[col]])
  }
  
  return(df)
}

# ---- Function: Create Histogram of Response Variables ----

hist_df <- function(df, cols) {
  if (is.numeric(cols)) {
    cols <- names(df)[cols]
  }
  
  for (col in cols) {
    hist(df[[col]], main = paste0("Histogram of ", col), xlab = col)
  }
}
