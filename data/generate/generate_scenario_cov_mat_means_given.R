generate_scenario_cov_mats_means_given <- function(cov_mats, means, n_per_class) {
  p <- nrow(cov_mats[[1]])
  n_classes <- ncol(means)
  data_list <- list()
  if (length(cov_mats) == 1) { #LDA
    for (k in 1:n_classes) {
      class_data <- MASS::mvrnorm(n = n_per_class, mu = means[, k], Sigma = cov_mats[[1]])
      class_df <- as.data.frame(class_data)
      class_df$Y <- as.factor(k)
      data_list[[k]] <- class_df
    }
  } else { #QDA
    for (k in 1:n_classes) {
    class_data <- MASS::mvrnorm(n = n_per_class, mu = means[, k], Sigma = cov_mats[[k]])
    class_df <- as.data.frame(class_data)
    class_df$Y <- as.factor(k)
    data_list[[k]] <- class_df
    }
  }

  final_data <- do.call(rbind, data_list)
  return(as.data.frame(final_data))

}