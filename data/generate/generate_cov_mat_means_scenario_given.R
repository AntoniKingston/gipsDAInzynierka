generate_cov_mat_means_scenario_given <- function(scenario, p, n_classes, perms, lambda_dist, n_per_class = 50,  max_iterations = 40) {
  source("data/generate/cov_mat_gen_met.R")
  df <- p + 2
  psi <- 2.0
  max_iterations <- 40

  # Target Accuracies
  signal_strength <- 0.6
  baseline_accuracy <- 1 / n_classes
  target_test_accuracy <- baseline_accuracy + (1 - baseline_accuracy) * signal_strength
  target_train_accuracy <- min(0.90, target_test_accuracy + 0.10)

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
  if (lambda_dist == "chisq") {
    base_cov_matrices <- generate_lamb_q(n_matrices = n_matrices, dim = p, lambda_dist = lambda_dist, df = df)
  }
  else {
    base_cov_matrices <- generate_lamb_q(n_matrices = n_matrices, dim = p, lambda_dist = lambda_dist)
  }

  if (is_gips) {
    base_cov_matrices <- mapply(gips::project_matrix, base_cov_matrices, perms, SIMPLIFY = FALSE)
  }
  for (i in 1:max_iterations) {
    class_means <- matrix(runif(n_classes * p, min = 0, max = 1), nrow = p, ncol = n_classes)
    scaled_cov_matrices <- lapply(base_cov_matrices, function(x) x * psi / (2 ** i))
    data_list <- mapply(function(k, cov_matrix) {
      class_data <- MASS::mvrnorm(n = n_per_class, mu = class_means[, k], Sigma = cov_matrix)
      class_df <- as.data.frame(class_data)
      class_df$Y <- as.factor(k)
      return(class_df)
    },
    1:n_classes,
    scaled_cov_matrices,
    SIMPLIFY = FALSE
    )
    final_data <- do.call(rbind, data_list)
    train_data <- final_data[train_indices, ]
    test_data <- final_data[-train_indices, ]

    classifier <- model_function(Y ~ ., data = train_data)
    train_predictions <- predict(classifier, train_data)$class
    train_accuracy <- mean(train_predictions == train_data$Y)
    test_predictions <- predict(classifier, test_data)$class
    test_accuracy <- mean(test_predictions == test_data$Y)

    is_train_acc_met <- train_accuracy > target_train_accuracy
    is_test_acc_met  <- test_accuracy > target_test_accuracy

    if (is_train_acc_met && is_test_acc_met) {
      break
    }
  }
  return(list("matrices" = scaled_cov_matrices, "means" = class_means))
}