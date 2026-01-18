library(gipsDA)
library(ggplot2)

source("data/generate/generate_scenario_cov_mat_means_given.R")

accuracy_experiment <- function(cov_mats, means, n_per_class, model, tr_ts_split = 0.7,
                                MAP = TRUE, opt = "BF", max_iter = 100) {
  experiment_data <- generate_scenario_cov_mats_means_given(cov_mats, means, n_per_class)
  train_idx <- sample(1:nrow(experiment_data), ceiling(tr_ts_split * nrow(experiment_data)))
  train_data <- experiment_data[train_idx, ]
  test_data <- experiment_data[-train_idx, ]

  classifier <- tryCatch({
    if (model == "lda") {
      MASS::lda(Y ~ ., data = train_data, method = "moment")
    } else if (model == "qda") {
      gipsDA:::qdamod.formula(Y ~ ., data = train_data, method = "moment")
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
  if (model == "qda") {
    pred <- gipsDA:::predict.qdamod(classifier, test_data)$class
  } else {
    pred <- predict(classifier, test_data)$class
  }
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
    mean(replicate(n_experiments, accuracy_experiment(cov_mats, means, n_per_class, model, tr_ts_split, MAP, opt, max_iter)))
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
  return(list("plot" = multiple_plots_info, "meta" = scenarios_metadata))
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


# generate_multiple_plots_info <- function(data_path,
#                              scenario_names = c("lda", "qda", "gipslda", "gipsqda", "gipsmultqda"),
#                              model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
#                              granularity = 25,
#                              lb = 50,
#                              n_experiments = 5,
#                              opt = "BF",
#                              max_iter = 100,
#                              tr_ts_split = 0.7,
#                              MAP = TRUE,
#                              data_file_prefix) {
#   data_paths <- paste(data_path, glue::glue("/{data_file_prefix}_scenario_{scenario_names}.csv"), sep = "")
#   #read in with shuffling
#   datasets <- lapply(lapply(data_paths, read.csv), function(x) x[sample(nrow(x)),])
#   multiple_plots_info <- parallel::mclapply(datasets, function(x) generate_single_plot_info(x, model_names, granularity, lb, n_experiments, opt, max_iter, tr_ts_split, MAP = MAP))
#   names(multiple_plots_info) <- scenario_names
#   return(multiple_plots_info)
# }