library(gipsDA)
library(ggplot2)

source("data/generate/generate_scenario_cov_mat_means_given.R")
source("manual_tests/models/LDAmod.R")
source("manual_tests/models/QDAmod.R")

accuracy_experiment <- function(cov_mats, means, n_per_class, model, n_per_class_test,
                                MAP = TRUE, opt = "BF", max_iter = 100, vuc = TRUE) {
  train_data <- generate_scenario_cov_mats_means_given(cov_mats, means, n_per_class)
  test_data <- generate_scenario_cov_mats_means_given(cov_mats, means, n_per_class_test)

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
    if (vuc) {
      return(list("acc" = 0, "vuc" = 0))
    }
    return(0)
  }

  prediction <- tryCatch({
  predict(classifier, test_data)
  }, error = function(e) {
    warning(sprintf("Error during prediction with model '%s': %s", model, e$message))
    NULL
  })

  if (is.null(prediction)) {
    if (vuc) {
      return(list("acc" = 0, "vuc" = 0))
    }
    return(0)
  }

  pred <- prediction$class

  if (vuc) {
  post <- prediction$posterior

  return(list("acc" = mean(pred == test_data$Y), "vuc" = as.numeric(pROC::multiclass.roc(response = test_data$Y, predict = post)$auc)))
  }

  mean(pred == test_data$Y)
}


generate_accuracy_data <- function(cov_mats,
                                   means,
                                   ns_obs,
                                   model,
                                   n_test = 1000,
                                   n_experiments = 5,
                                   n_experiments_spe = 5,
                                   spe_idx = c(1,3,5),
                                   MAP = TRUE,
                                   opt = "BF",
                                   max_iter = 100) {
  accs_vucs <- lapply(ns_obs, function(n_obs) {
    n_per_class <- floor(n_obs / ncol(means))
    n_per_class_test <- floor(n_test / ncol(means))
    lapply(seq_len(n_experiments), function(i) {
      accuracy_experiment(cov_mats, means, n_per_class, model, n_per_class_test, MAP, opt, max_iter)
    })
  })
  flat <- unlist(accs_vucs, recursive = FALSE)
  accs <- vapply(flat, `[[`, numeric(1), "acc")
  vucs <- vapply(flat, `[[`, numeric(1), "vuc")


  ns_obs_spe <- ns_obs[spe_idx]
  accs_spe <- as.numeric(unlist(lapply(ns_obs_spe, function(n_obs) {
    n_per_class <- floor(n_obs / ncol(means))
    n_per_class_test <- floor(n_test / ncol(means))
    replicate(n_experiments_spe, accuracy_experiment(cov_mats, means, n_per_class, model, n_per_class_test, MAP, opt, max_iter, vuc = FALSE))
  })))





  return(list("accs" = accs, "accs_spe" = accs_spe, "vucs" = vucs))
}

generate_single_plot_info <- function(scenario_metadata,
                                      model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                                      ns_obs,
                                      n_test = 1000,
                                      n_experiments = 5,
                                      n_experiments_spe = 5,
                                      spe_idx = c(1,3,5),
                                      MAP = TRUE,
                                      opt = "BF",
                                      max_iter = 100
                                      ) {

  cov_mats <- scenario_metadata$matrices
  means <- scenario_metadata$means
  plot_info <- lapply(model_names, function(model) {
    acc_data <- generate_accuracy_data(cov_mats, means, ns_obs, model, n_test, n_experiments, n_experiments_spe, spe_idx, MAP, opt, max_iter)
    list("accs" = acc_data[["accs"]], "accs_spe" = acc_data[["accs_spe"]], "vucs" = acc_data[["vucs"]], "ns_obs" = ns_obs)
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
                             n_experiments_spe = 5,
                             spe_idx = c(1,3,5),
                             opt = "BF",
                             max_iter = 100,
                             n_test = 1000,
                             MAP = TRUE) {
  source("data/generate/generate_cov_mat_means_scenario_given.R")
  scenarios_metadata <- lapply(scenario_names, function(scenario) {
    generate_cov_mat_means_scenario_given(scenario, p, n_classes, perms, lambda_dist, n_per_class)
  })
  ns_obs <- round(exp(seq(log(lb), log(n_classes*n_per_class), length.out = granularity)))
  multiple_plots_info <- lapply(scenarios_metadata, function(scenario_metadata) {
    generate_single_plot_info(scenario_metadata, model_names, ns_obs, n_test, n_experiments, n_experiments_spe, spe_idx, MAP, opt, max_iter)
  })
  names(multiple_plots_info) <- scenario_names
  names(scenarios_metadata) <- scenario_names

  test_pairs <- list(c("lda", "gipsldacl"), c("lda", "gipsldawa"), c("gipsldacl", "gipsldawa"), c("qda", "gipsqda"), c("qda", "gipsmultqda"))
  test_info <- lapply(scenario_names, function(name) {
    ret_list <- lapply(test_pairs, function(test_pair) {
      wilcoxon_test(multiple_plots_info, name, test_pair[1], test_pair[2], n_experiments_spe, spe_idx)
    })
    names(ret_list) <- lapply(test_pairs, function(pair) {
        return(paste(pair, collapse = " vs "))
      })
    return(ret_list)
  })

  names(test_info) <- scenario_names


  return(list("plot" = multiple_plots_info, "meta" = scenarios_metadata, "test" = test_info))
}

# ====== Plotting functions ======

create_multilevel_plot <- function(
  mult_plots_info,
  metric_name = "accs",  # "accs" or "vucs"
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

      if (is.null(data_points[[metric_name]])) {
        stop(paste("Metric", metric_name, "not found for model", model_name))
      }

      le <- length(data_points[["ns_obs"]])

      raw_values <- data_points[[metric_name]]
      n_experiments <- length(raw_values) / le

      section_lengths <- c(100, n_experiments, 100, n_experiments, 100)

      mean_values <- tapply(raw_values, rep(seq_len(le), each = n_experiments), mean)

      temp_df <- data.frame(
        observations = data_points[["ns_obs"]],
        accuracy = mean_values,
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

create_multilevel_boxplot <- function(
  mult_plots_info,
  spe_idx,
  ncol = NULL,
  xlab = "Number of Observations",
  ylab = "Accuracy",
  point_size = 1,
  scales = "fixed"
) {
  stopifnot(is.list(mult_plots_info))

  data_list <- list()

  for (dataset_name in names(mult_plots_info)) {
    inner_list <- mult_plots_info[[dataset_name]]
    for (model_name in names(inner_list)) {
      data_points <- inner_list[[model_name]]

      obs_vec <- data_points[["ns_obs"]]
      acc_vec <- data_points[["accs_spe"]]

      le <- length(obs_vec)
      n_experiments <- length(acc_vec) / le

      acc_matrix <- matrix(acc_vec, nrow = n_experiments,
                           ncol = le, byrow = FALSE)
      selected_accs <- acc_matrix[, spe_idx, drop = FALSE]
      selected_obs <- obs_vec[spe_idx]

      obs_for_df <- rep(selected_obs, each = n_experiments)
      accs_for_df <- as.vector(selected_accs)

      temp_df <- data.frame(
        observations = as.factor(obs_for_df),
        accuracy = accs_for_df,
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
    aes(x = observations, y = accuracy, fill = model)
  ) +
  geom_boxplot(outlier.size = point_size, alpha = 0.7) +
  facet_wrap(~ dataset, ncol = ncol, scales = scales) +
  labs(
    x = xlab,
    y = ylab,
    fill = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    text = element_text(size = 12)
  )

  return(gg_plot)
}

# ====== Statistical tests ======

wilcoxon_test <- function(acc_info, scenario, mod1, mod2,
                          n_exp_spe = 5, spe_idx = c(1, 3, 5)) {

  x <- acc_info[[scenario]][[mod1]]$accs_spe
  y <- acc_info[[scenario]][[mod2]]$accs_spe

  stopifnot(length(x) == length(y))
  stopifnot(length(x) == n_exp_spe * length(spe_idx))

  pvals <- lapply(seq_along(spe_idx), function(k) {
    idx <- ((k - 1) * n_exp_spe + 1):(k * n_exp_spe)
    wilcox.test(x[idx], y[idx], alternative = "less", exact = FALSE)$p.value
  })

  names(pvals) <- as.character(spe_idx)
  pvals
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
                                                n_experiments_spe = 5,
                                                spe_idx = c(1, 3, 5),
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

  splits_main <- lapply(ns_obs, function(n) {
    generate_splits_real_data(n, nrow(scenario_data), tr_ts_split, n_experiments)
  })

  ns_obs_spe <- ns_obs[spe_idx]
  splits_spe <- lapply(ns_obs_spe, function(n) {
    generate_splits_real_data(n, nrow(scenario_data), tr_ts_split, n_experiments_spe)
  })

  task_queue <- list()
  for (model in model_names) {
    for (i in seq_along(ns_obs)) {
      s_list <- splits_main[[i]]
      for (j in seq_along(s_list)) {
        task_queue[[length(task_queue) + 1]] <- list(
          type = "main", model = model, n_obs = ns_obs[i], split = s_list[[j]],
          n_idx = i, exp_idx = j
        )
      }
    }
    for (k in seq_along(spe_idx)) {
      s_list <- splits_spe[[k]]
      for (j in seq_along(s_list)) {
        task_queue[[length(task_queue) + 1]] <- list(
          type = "spe", model = model, n_obs = ns_obs_spe[k], split = s_list[[j]],
          spe_k_idx = k, exp_idx = j
        )
      }
    }
  }

  task_queue <- task_queue[sample(length(task_queue))]

  results_flat <- parallel::mclapply(task_queue, function(task) {
    calc_vuc <- (task$type == "main")
    res <- accuracy_experiment_real_data(scenario_data, task$split, task$model, MAP, opt, max_iter, vuc = calc_vuc)
    c(task, list(res_acc = res$acc, res_vuc = res$vuc))
  }, mc.cores = n_cores, mc.preschedule = FALSE)

  plot_info <- setNames(vector("list", length(model_names)), model_names)

  for (m in model_names) {
    res_m <- Filter(function(x) x$model == m, results_flat)

    res_main <- Filter(function(x) x$type == "main", res_m)
    res_main <- res_main[order(sapply(res_main, `[[`, "n_idx"), sapply(res_main, `[[`, "exp_idx"))]

    res_spe <- Filter(function(x) x$type == "spe", res_m)
    res_spe <- res_spe[order(sapply(res_spe, `[[`, "spe_k_idx"), sapply(res_spe, `[[`, "exp_idx"))]

    plot_info[[m]] <- list(
      "accs" = sapply(res_main, `[[`, "res_acc"),
      "vucs" = sapply(res_main, `[[`, "res_vuc"),
      "accs_spe" = sapply(res_spe, `[[`, "res_acc"),
      "ns_obs" = ns_obs
    )
  }

  acc_info_wrapper <- list("real_data" = plot_info)

  test_info <- lapply(test_pairs, function(pair) {
    wilcoxon_test(
      acc_info = acc_info_wrapper,
      scenario = "real_data",
      mod1 = pair[1],
      mod2 = pair[2],
      n_exp_spe = n_experiments_spe,
      spe_idx = spe_idx
    )
  })

  names(test_info) <- lapply(test_pairs, function(pair) paste(pair, collapse = " vs "))

  return(list(
    plot_info = plot_info,
    test_info = test_info
  ))
}

generate_splits_real_data <- function(n, total_rows, tr_ts_split, n_experiments) {
  replicate(n_experiments, {
    subset_idx <- sample(seq_len(total_rows), size = total_rows)

    train_size <- floor(n)
    train_idx <- sample(subset_idx, size = train_size)
    test_idx  <- setdiff(subset_idx, train_idx)

    list(train = train_idx, test = test_idx)
  }, simplify = FALSE)
}

accuracy_experiment_real_data <- function(df, split, model, MAP = TRUE, opt = "BF", max_iter = 100, vuc = TRUE) {
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

  if (is.null(classifier)) return(list(acc = 0, vuc = 0))

  prediction <- predict(classifier, test_data)
  acc <- mean(prediction$class == test_data$Y)

  roc_val <- 0
  if (vuc) {
    tryCatch({
      roc_res <- pROC::multiclass.roc(response = test_data$Y, predictor = prediction$posterior)
      roc_val <- as.numeric(roc_res$auc)
    }, error = function(e) { roc_val <<- 0 })
  }

  return(list(acc = acc, vuc = roc_val))
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
