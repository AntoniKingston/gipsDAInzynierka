library(gipsDA)
generate_cov_mat_means_scenario_given.R <- function(scenario, p, n_classes, perms, lambda_dist, n_per_class = 50,  max_iterations = 40) {
  source("data/generate/cov_mat_gen_met.R")

  df <- p + 2
  psi <- 2.0
  max_iterations <- 40
  target_train_accuracy <- 0.70
  target_test_accuracy  <- 0.50
  train_ratio <- 0.6
  n_tot <- n_classes * n_per_class
  train_indices <- sample(1:n_tot, size = floor(train_ratio * n_tot))

  is_lda <- scenario %in% c("lda", "gipslda")
  is_gips <- scenario %in% c("gipslda", "gipsqda", "gipsmultqda")

  model_function <- ifelse(is_lda, MASS::lda, MASS::qda)

  if (!scenario == "gipsqda") {
    perms <- perms[1]
  }

  n_matrices <- ifelse(is_lda, 1, n_classes)
  base_cov_matrices <- generate_lamb_q(n_matrices = n_matrices, dim = p, lambda_dist = lambda_dist)


  if (is_gips) {
    base_cov_matrices <- mapply(gips::project_matrix, base_cov_matrices, perms)
  }
  for (i in 1:max_terations) {
    class_means <- matrix(runif(n_classes * p, min = 0, max = 1), nrow = p, ncol = n_classes)
    scaled_cov_matrices <- lapply(base_cov_matrices, function(x) x * psi / (2 ** i))
    data_list <- mapply(function(k, cov_matrix) {
      class_data <- MASS::mvrnorm(n = n_per_class, mu = class_means[, k], Sigma = cov_matrix)
      class_df <- as.data.frame(class_data)
      class_df$Y <- as.factor(k)
      data_list[[k]] <- class_df
    },
    1:n_classes,
    scaled_cov_matrices
    )
    final_data <- do.call(rbind, data_list)
    train_data <- final_data[train_indices, ]
    test_data <- final_data[-train_indices, ]

    classifier <- model_function(Y ~ ., data = train_data)
    train_predictions <- predict(classifier, train_data)$class
    train_accuracy <- mean(train_predictions == train_data$Y)
    test_predictions <- predict(classifier, test_data)$class
    test_accuracy <- mean(test_predictions == test_data$Y)

    is_train_acc_met <- lda_train_accuracy > target_train_accuracy
    is_test_acc_met  <- lda_test_accuracy > target_test_accuracy

    if (is_train_acc_met && is_test_acc_met) {
      break
    }
  }
  return(list("matrices" = scaled_cov_matrices, "means" = class_means))
}








# if (scenario == "lda") {
#     for (i in 1:max_iterations) {
#       cov_matrix <- base_cov_matrix * psi / (2**i)
#
#       class_means <- matrix(runif(n_classes * p, min = 0, max = 1), nrow = p, ncol = n_classes)
#       data_list <- list()
#       for (k in 1:n_classes) {
#         class_data <- MASS::mvrnorm(n = n_per_class, mu = class_means[, k], Sigma = cov_matrix)
#         class_df <- as.data.frame(class_data)
#         class_df$Y <- as.factor(k)
#         data_list[[k]] <- class_df
#       }
#       final_data <- do.call(rbind, data_list)
#
#       train_indices <- sample(1:nrow(final_data), size = floor(train_ratio * nrow(final_data)))
#       train_data <- final_data[train_indices, ]
#       test_data <- final_data[-train_indices, ]
#
#       lda_model <- MASS::lda(Y ~ ., data = train_data)
#       lda_train_predictions <- predict(lda_model, train_data)$class
#       lda_train_accuracy <- mean(lda_train_predictions == train_data$Y)
#       lda_test_predictions <- predict(lda_model, test_data)$class
#       lda_test_accuracy <- mean(lda_test_predictions == test_data$Y)
#
#
#       is_train_acc_met <- lda_train_accuracy > target_train_accuracy
#       is_test_acc_met  <- lda_test_accuracy > target_test_accuracy
#
#       if (is_train_acc_met && is_test_acc_met) {
#         return(list("matrices" = list(cov_matrix)), "means" = class_means)
#       }
#     }
#   } else if (scenario == "qda") {
#
#
#
#   } else if (scenario == "gipslda") {
#
#   } else if (scenario == "gipsqda") {
#
#   } else if (scenario == "gipsmultqda") {
#
#   } else {
#      rlang::abort(message = "ERROR wrong scenario name")
#   }