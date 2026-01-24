library(gipsDA)
library(ggplot2)

source("data/generate/generate_scenario_cov_mat_means_given.R")
source("manual_tests/models/LDAmod.R")
source("manual_tests/models/QDAmod.R")

accuracy_experiment <- function(cov_mats, means, n_per_class, model, tr_ts_split = 0.7,
                                MAP = TRUE, opt = "BF", max_iter = 100) {
  experiment_data <- generate_scenario_cov_mats_means_given(cov_mats, means, n_per_class)
  train_idx <- sample(1:nrow(experiment_data), ceiling(tr_ts_split * nrow(experiment_data)))
  train_data <- experiment_data[train_idx, ]
  test_data <- experiment_data[-train_idx, ]

  classifier <- tryCatch({
    if (model == "lda") {
      ldamod(Y ~ ., data = train_data)
    } else if (model == "qda") {
      qdamod(Y ~ ., data = train_data)
    } else if (model == "gipsldacl") {
      gipslda(Y ~ ., data = train_data, weighted_avg = FALSE, MAP = MAP, optimizer = opt, max_iter = max_iter)
    } else if (model == "gipsldawa") {
      gipslda(Y ~ ., data = train_data, weighted_avg = TRUE, MAP = MAP, optimizer = opt, max_iter = max_iter)
    } else if (model == "gipsqda") {
      gipsqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
    } else {
      gipsmultqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
    }
  }, error = function(e) {
    warning(sprintf("Error fitting model: %s ", model))
    NULL
  })

  if (is.null(classifier)) {
    return(0)
  }

  pred <- predict(classifier, test_data)$class

  mean(pred == test_data$Y)
}


generate_accuracy_data <- function(cov_mats,
                                   means,
                                   ns_obs,
                                   model,
                                   tr_ts_split = 0.7,
                                   n_experiments = 5,
                                   MAP = TRUE,
                                   opt = "BF",
                                   max_iter = 100) {
  accs <- as.numeric(unlist(lapply(ns_obs, function(n_obs) {
    n_per_class <- floor(n_obs / ncol(means))
    replicate(n_experiments, accuracy_experiment(cov_mats, means, n_per_class, model, tr_ts_split, MAP, opt, max_iter))
  })))




  return(accs)
}

generate_single_plot_info <- function(scenario_metadata,
                                      model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                                      ns_obs,
                                      tr_ts_split = 0.7,
                                      n_experiments = 5,
                                      MAP = TRUE,
                                      opt = "BF",
                                      max_iter = 100
                                      ) {

  cov_mats <- scenario_metadata$matrices
  means <- scenario_metadata$means

  plot_info <- lapply(model_names, function(model) {
    list("accs" = generate_accuracy_data(cov_mats, means, ns_obs, model, tr_ts_split, n_experiments, MAP, opt, max_iter), "ns_obs" = ns_obs)
  })
  names(plot_info) <- model_names
  plot_info
}

generate_multiple_plots_info_qr <- function(p,
                             n_classes,
                             perms,
                             lambda_dist,
                             n_per_class = 50,
                             scenario_names = c("qda", "gipsqda", "gipsmultqda", "lda", "gipslda"),
                             model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                             granularity = 25,
                             lb = 16,
                             n_experiments = 5,
                             opt = "BF",
                             max_iter = 100,
                             tr_ts_split = 0.7,
                             MAP = TRUE) {
  source("data/generate/generate_cov_mat_means_scenario_given.R")

  scenarios_metadata <- lapply(scenario_names, function(scenario) {
    generate_cov_mat_means_scenario_given(scenario, p, n_classes, perms, lambda_dist, n_per_class)
  })
  ns_obs <- round(exp(seq(log(lb), log(n_classes*n_per_class), length.out = granularity)))
  multiple_plots_info <- lapply(scenarios_metadata, function(scenario_metadata) {
    generate_single_plot_info(scenario_metadata, model_names, ns_obs, tr_ts_split, n_experiments, MAP, opt, max_iter)
  })

  names(multiple_plots_info) <- scenario_names
  names(scenarios_metadata) <- scenario_names


  test_pairs <- list(c("lda", "gipsldacl"), c("lda", "gipsldawa"), c("gipsldacl", "gipsldawa"), c("qda", "gipsqda"), c("qda", "gipsmultqda"))
  test_info <- lapply(scenario_names, function(name) {
    ret_list <- lapply(test_pairs, function(test_pair) {
      perm_test_blocked(multiple_plots_info, name, test_pair[1], test_pair[2])
    })
    names(ret_list) <- lapply(test_pairs, function(pair) {
        return(paste(pair, collapse = " vs "))
      })
    return(ret_list)
  })

  names(test_info) <- scenario_names


  return(list("plot" = multiple_plots_info, "meta" = scenarios_metadata, "test" = test_info))
}


create_multilevel_plot <- function(
  mult_plots_info,
  ncol = NULL,
  xlab = "Number of Observations",
  ylab = "Accuracy",
  line_size = 0.5,
  point_size = 3
) {
  stopifnot(is.list(mult_plots_info))

  data_list <- list()

  for (dataset_name in names(mult_plots_info)) {
    inner_list <- mult_plots_info[[dataset_name]]
    for (model_name in names(inner_list)) {
      data_points <- inner_list[[model_name]]
      le <- length(data_points[["ns_obs"]])
      n_experiments <- length(data_points[["accs"]]) / le

      data_points[["accs"]] <- tapply(data_points[["accs"]], rep(seq_len(le), each = n_experiments), mean)

      temp_df <- data.frame(
        observations = data_points[[2]],
        accuracy = data_points[[1]],
        model = model_name,
        dataset = dataset_name
      )
      data_list[[length(data_list) + 1]] <- temp_df
    }
  }

  if (length(data_list) == 0) {
    stop("Input 'mult_plots_info' is empty or has an invalid structure.")
  }

  plot_df <- do.call(rbind, data_list)

  plot_df$dataset <- factor(plot_df$dataset, levels = names(mult_plots_info))

  gg_plot <- ggplot(
    plot_df,
    aes(x = observations, y = accuracy, color = model, group = model)
  ) +
  geom_line(linewidth = line_size) +
  facet_wrap(~ dataset, ncol = ncol) +
  labs(
    x = xlab,
    y = ylab,
    color = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(0, 1))

  return(gg_plot)
}

perm_test_blocked <- function(acc_info,
                              scenario,
                              mod1,
                              mod2,
                              B = 5000) {
  x <- acc_info[[scenario]][[mod1]]$accs
  y <- acc_info[[scenario]][[mod2]]$accs
  n_obs_vals <- acc_info[[scenario]][[mod1]]$ns_obs

  K <- length(n_obs_vals)
  stopifnot(length(x) == length(y))
  stopifnot(length(x) %% K == 0)

  n_exp <- length(x) / K
  n_obs <- rep(n_obs_vals, each = n_exp)


  obs_stat <- mean(tapply(y - x, n_obs, mean))


  perm_stats <- replicate(B, {
    y_perm <- y

    for (n in n_obs_vals) {
      idx <- which(n_obs == n)
      swap <- rbinom(length(idx), 1, 0.5) == 1

      tmp <- y_perm[idx][swap]
      y_perm[idx][swap] <- x[idx][swap]
      x[idx][swap] <- tmp
    }

    mean(tapply(y_perm - x, n_obs, mean))
  })

  p_value <- mean(perm_stats >= obs_stat)

  return(p_value)
}

# ======================================================================
#                          Real Data Part
# ======================================================================

generate_single_plot_info_real_data <- function(scenario_data,
                                                model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                                                granularity = 25,
                                                lb = 50,
                                                up = 100,
                                                n_experiments = 5,
                                                MAP = TRUE,
                                                opt = "BF",
                                                max_iter = 100,
                                                tr_ts_split = 0.7,
                                                n_cores = parallel::detectCores() - 1,
                                                test_pairs = list(c("lda", "gipsldacl"),
                                                                  c("lda", "gipsldawa"),
                                                                  c("gipsldacl", "gipsldawa"),
                                                                  c("qda", "gipsqda"),
                                                                  c("qda", "gipsmultqda"))) {

  ns_obs <- round(exp(seq(log(lb), log(up), length.out = granularity)))

  # Generate splits sequentially (fast operation)
  splits_by_n <- lapply(ns_obs, function(n) {
    generate_splits_real_data(n, nrow(scenario_data),
                              tr_ts_split, n_experiments)
  })

  # Flatten tasks into a single queue (Model x N x Split)
  task_queue <- list()
  for (model in model_names) {
    for (i in seq_along(ns_obs)) {
      current_n <- ns_obs[i]
      current_splits <- splits_by_n[[i]]

      for (j in seq_along(current_splits)) {
        task_queue[[length(task_queue) + 1]] <- list(
          model = model,
          n_obs = current_n,
          split = current_splits[[j]],
          n_idx = i,
          exp_idx = j
        )
      }
    }
  }

  # Shuffle tasks for load balancing
  task_queue <- task_queue[sample(length(task_queue))]

  # Run parallel execution using dynamic scheduling
  results_flat <- parallel::mclapply(task_queue, function(task) {
    acc <- accuracy_experiment_real_data(
      df = scenario_data,
      split = task$split,
      model = task$model,
      MAP = MAP,
      opt = opt,
      max_iter = max_iter
    )
    list(model = task$model, n_idx = task$n_idx, exp_idx = task$exp_idx, acc = acc)
  }, mc.cores = n_cores, mc.preschedule = FALSE)

  # Aggregate results back to the original structure
  plot_info <- setNames(vector("list", length(model_names)), model_names)

  for (m in model_names) {
    model_results <- Filter(function(x) x$model == m, results_flat)

    all_accs <- matrix(NA, nrow = n_experiments, ncol = length(ns_obs))

    for (res in model_results) {
      all_accs[res$exp_idx, res$n_idx] <- res$acc
    }
    mean_accs <- colMeans(all_accs, na.rm = TRUE)

    plot_info[[m]] <- list(
      "accs" = mean_accs,
      "ns_obs" = ns_obs,
      "all_accs" = all_accs
    )
  }
  # Prepare data for permutation tests
  wrapper_for_test <- list("real_data" = plot_info)

  test_info <- lapply(test_pairs, function(pair) {
    perm_test_blocked(wrapper_for_test, "real_data", pair[1], pair[2])
  })

  names(test_info) <- lapply(test_pairs,
                             function(pair) paste(pair, collapse = " vs "))
  return(list(
    plot_info = plot_info,
    test_info = test_info
  ))
}

generate_splits_real_data <- function(n, total_rows, tr_ts_split, n_experiments) {
  replicate(n_experiments, {
    subset_idx <- sample(seq_len(total_rows), size = n)

    train_size <- floor(tr_ts_split * n)
    train_idx <- sample(subset_idx, size = train_size)
    test_idx  <- setdiff(subset_idx, train_idx)

    list(train = train_idx, test = test_idx)
  }, simplify = FALSE)
}

accuracy_experiment_real_data <- function(df, split, model, MAP = TRUE, opt = "BF", max_iter = 100) {
  train_data <- df[split$train, , drop = FALSE]
  test_data  <- df[split$test,  , drop = FALSE]

  classifier <- tryCatch({
    if (model == "lda") {
      ldamod(Y ~ ., data = train_data)
    } else if (model == "qda") {
      qdamod(Y ~ ., data = train_data)
    } else if (model == "gipsldacl") {
      gipslda(Y ~ ., data = train_data, weighted_avg = FALSE, MAP = MAP, optimizer = opt, max_iter = max_iter)
    } else if (model == "gipsldawa") {
      gipslda(Y ~ ., data = train_data, weighted_avg = TRUE, MAP = MAP, optimizer = opt, max_iter = max_iter)
    } else if (model == "gipsqda") {
      gipsqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
    } else {
      gipsmultqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
    }
  }, error = function(e) {
    message(sprintf("⚠️ Error in model '%s': %s", model, e$message))
    return(NULL)
  })

  if (is.null(classifier)) return(0)

  pred <- predict(classifier, test_data)$class
  mean(pred == test_data$Y)
}

remove_low_variance_columns <- function(df, threshold = 0.95, target_col = "Y") {
  # Temporarily remove target column to protect it
  target_data <- NULL
  if (target_col %in% names(df)) {
    target_data <- df[[target_col]]
    df[[target_col]] <- NULL
  }

  # Check dominance ratio for each column
  keep_col <- sapply(df, function(x) {
    x <- na.omit(x)
    if (length(x) == 0 || length(unique(x)) <= 1) return(FALSE)

    # Calculate frequency of the most common value
    (max(table(x)) / length(x)) < threshold
  })

  # Log and filter
  dropped_count <- sum(!keep_col)
  if (dropped_count > 0) {
    message(sprintf("Dropped %d low-variance columns (dominance > %.0f%%).",
                    dropped_count, threshold * 100))
  }

  df <- df[, keep_col, drop = FALSE]

  # Restore target column
  if (!is.null(target_data)) {
    df[[target_col]] <- target_data
  }

  return(df)
}

fix_tiny_values <- function(df, target_col = "Y") {

  # Identify numeric predictors to check
  cols_to_check <- setdiff(names(df), target_col)
  df_fixed <- df

  for (col in cols_to_check) {
    vals <- df[[col]]

    # Skip non-numeric or empty columns
    if (!is.numeric(vals) || length(vals) == 0) next

    # Check the order of magnitude
    avg_val <- mean(abs(vals), na.rm = TRUE)

    # Scale up if values are too small (avoiding numerical underflow)
    if (!is.na(avg_val) && avg_val > 0 && avg_val < 0.01) {
      magnitude <- floor(log10(avg_val))
      multiplier <- 10 ^ (-magnitude)

      df_fixed[[col]] <- vals * multiplier
    }
  }

  return(df_fixed)
}

remove_collinear_features <- function(df, target_col = "Y", cutoff = 0.99) {
  y <- df[[target_col]]
  x <- df[, setdiff(names(df), target_col)]

  non_zero_var <- sapply(x, function(col) var(col, na.rm = TRUE) > 0)
  x <- x[, non_zero_var]

  corr_matrix <- cor(x, use = "pairwise.complete.obs")

  diag(corr_matrix) <- 0

  corr_matrix[lower.tri(corr_matrix)] <- 0

  cols_to_remove_idx <- which(apply(corr_matrix,
                                    2, function(col) any(abs(col) > cutoff)))

  if (length(cols_to_remove_idx) > 0) {
    message(sprintf("Removing %d collinear columns (corr > %.2f)",
                    length(cols_to_remove_idx), cutoff))
    x <- x[, -cols_to_remove_idx]
  }

  x[[target_col]] <- y
  return(x)
}

clean_infinite_values <- function(df) {
  df[] <- lapply(df, function(x) {
    if(is.numeric(x)) {
      x[is.infinite(x)] <- NA
    }
    return(x)
  })
  df <- na.omit(df)

  return(df)
}
