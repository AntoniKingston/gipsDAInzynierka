library(gipsDA)
library(ggplot2)

generate_splits <- function(n, tr_ts_split, n_experiments) {
  replicate(
    n_experiments,
    {
      train_idx <- sample(seq_len(n), size = floor(tr_ts_split * n))
      list(
        train = train_idx,
        test  = setdiff(seq_len(n), train_idx)
      )
    },
    simplify = FALSE
  )
}

accuracy_experiment <- function(df, split, model,
                                MAP = TRUE, opt = "BF", max_iter = 100) {

  train_data <- df[split$train, , drop = FALSE]
  test_data  <- df[split$test,  , drop = FALSE]

  classifier <- tryCatch({
    if (model == "lda") {
      MASS::lda(Y ~ ., data = train_data, method = "moment")
    } else if (model == "qda") {
      MASS::qda(Y ~ ., data = train_data, method = "moment")
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

  # if (model == "lda") {
  #   classifier <- MASS::lda(Y ~ ., data = train_data, method = "moment")
  # } else if (model == "qda") {
  #   classifier <- MASS::qda(Y ~ ., data = train_data, method = "moment")
  # } else if (model == "gipsldacl") {
  #   classifier <- gipslda(Y ~ ., data = train_data,
  #                         weighted_avg = FALSE,
  #                         MAP = MAP, optimizer = opt, max_iter = max_iter)
  # } else if (model == "gipsldawa") {
  #   classifier <- gipslda(Y ~ ., data = train_data,
  #                         weighted_avg = TRUE,
  #                         MAP = MAP, optimizer = opt, max_iter = max_iter)
  # } else if (model == "gipsqda") {
  #   classifier <- gipsqda(Y ~ ., data = train_data,
  #                         MAP = MAP, optimizer = opt, max_iter = max_iter)
  # } else {
  #   classifier <- gipsmultqda(Y ~ ., data = train_data,
  #                             MAP = MAP, optimizer = opt, max_iter = max_iter)
  # }

  if (is.null(classifier)) {
    return(0)
  }

  pred <- predict(classifier, test_data)$class
  mean(pred == test_data$Y)
}


# accuracy_experiment <- function(train_data, test_data, model, MAP = TRUE, opt = "BF", max_iter = 100, tr_ts_split = 0.7) {
#   # classifier <- tryCatch({
#   #   if (model == "lda") {
#   #     MASS::lda(Y ~ ., data = train_data, method = "mve")
#   #   } else if (model == "qda") {
#   #     MASS::qda(Y ~ ., data = train_data, method = "mve")
#   #   } else if (model == "gipsldacl") {
#   #     gipslda(Y ~ ., data = train_data, weighted_avg = FALSE, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   #   } else if (model == "gipsldawa") {
#   #     gipslda(Y ~ ., data = train_data, weighted_avg = TRUE, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   #   } else if (model == "gipsqda") {
#   #     gipsqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   #   } else {
#   #     gipsmultqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   #   }
#   # }, error = function(e) {
#   #   warning(sprintf("Error fitting model: %s ", model))
#   #   NULL
#   # })
#
#   if (model == "lda") {
#     classifier <- MASS::lda(Y ~ ., data = train_data, method = "moment")
#   } else if (model == "qda") {
#     classifier <- MASS::qda(Y ~ ., data = train_data, method = "moment")
#   } else if (model == "gipsldacl") {
#     classifier <- gipslda(Y ~ ., data = train_data, weighted_avg = FALSE, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   } else if (model == "gipsldawa") {
#     classifier <- gipslda(Y ~ ., data = train_data, weighted_avg = TRUE, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   } else if (model == "gipsqda") {
#     classifier <- gipsqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   } else {
#     classifier <- gipsmultqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
#   }
#
#
#   if (is.null(classifier)) {
#     return(0)
#   }
#
#   pred <- predict(classifier, test_data)$class
#   y_true <- test_data$Y
#   acc <- mean(pred == y_true)
#   return(acc)
# }

generate_accuracy_data <- function(df,
                                   model,
                                   splits_per_n,
                                   granularity = 25,
                                   lb = 50,
                                   n_experiments = 5,
                                   MAP = TRUE,
                                   opt = "BF",
                                   max_iter = 100) {

  if (nrow(df) < lb) {
    rlang::abort("dataset has less rows than specified lower bound")
  }

  accs <- as.numeric(unlist(lapply(splits_per_n, function(splits) {
    mean(vapply(splits, function(split) {
      accuracy_experiment(df, split, model, MAP = MAP, opt = opt, max_iter = max_iter)
    }, numeric(1)))
  })))

  return(accs)
}


# generate_accuracy_data <- function(df,
#                                    model,
#                                    granularity = 25,
#                                    lb = 50,
#                                    n_experiments = 5,
#                                    MAP = TRUE,
#                                    opt = "BF",
#                                    max_iter = 100,
#                                    tr_ts_split = 0.7) {
#   if(nrow(df) < lb) {
#   rlang::abort(c("x" = "dataset has less rows than specified lower bound"))
#   }
#
#   ns_obs <- round(exp(seq(log(lb), log(nrow(df)), length.out = granularity)))
#   accs <- c()
#   for (n_obs in ns_obs) {
#     df_temp <- df[1:n_obs,]
#     train_indexes = sample(1:nrow(df_temp), size = tr_ts_split * nrow(df_temp))
#     train_data = df_temp[train_indexes, ]
#     test_data = df_temp[-train_indexes, ]
#     accs <- c(accs, mean(rep(accuracy_experiment(train_data, test_data, model, MAP = MAP, opt = opt, max_iter = max_iter, tr_ts_split = 0.7), n_experiments)))
#   }
#
#   return(list("accs" = accs, "ns_obs" = ns_obs))
# }

generate_single_plot_info <- function(scenario_data,
                                      model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                                      granularity = 25,
                                      lb = 50,
                                      n_experiments = 5,
                                      MAP = TRUE,
                                      opt = "BF",
                                      max_iter = 100,
                                      tr_ts_split = 0.7) {

  ns_obs <- round(exp(seq(log(lb), log(nrow(scenario_data)), length.out = granularity)))

  splits_per_n <- lapply(ns_obs, function(n_obs) {
    generate_splits(n_obs, tr_ts_split, n_experiments)
  })

  plot_info <- lapply(model_names, function(model) {
    list("accs" = generate_accuracy_data(scenario_data, model, splits_per_n, granularity, lb, n_experiments, MAP, opt, max_iter), "ns_obs" = ns_obs)
  })
  names(plot_info) <- model_names
  plot_info
}

# generate_single_plot_info <- function(scenario_data,
#                                       model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
#                                       granularity = 25,
#                                       lb = 50,
#                                       n_experiments = 5,
#                                       MAP = TRUE,
#                                       opt = "BF",
#                                       max_iter = 100,
#                                       tr_ts_split = 0.7) {
#   plot_info <- lapply(model_names, function (x) generate_accuracy_data(scenario_data, x, granularity, lb, n_experiments, opt, max_iter, tr_ts_split, MAP = MAP))
#   names(plot_info) <- model_names
#   return(plot_info)
# }

generate_multiple_plots_info <- function(data_path,
                             scenario_names = c("lda", "qda", "gipslda", "gipsqda", "gipsmultqda"),
                             model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                             granularity = 25,
                             lb = 50,
                             n_experiments = 5,
                             opt = "BF",
                             max_iter = 100,
                             tr_ts_split = 0.7,
                             MAP = TRUE,
                             data_file_prefix) {
  set.seed(42)
  data_paths <- paste(data_path, glue::glue("/{data_file_prefix}_scenario_{scenario_names}.csv"), sep = "")
  #read in with shuffling
  datasets <- lapply(lapply(data_paths, read.csv), function(x) x[sample(nrow(x)),])
  multiple_plots_info <- parallel::mclapply(datasets, function(x) generate_single_plot_info(x, model_names, granularity, lb, n_experiments, opt, max_iter, tr_ts_split, MAP = MAP))
  names(multiple_plots_info) <- scenario_names
  return(multiple_plots_info)
}
## przykładowe wywołanie (działa jak się dobrze dane wygeneruje)
# mult_plots_info <- generate_multiple_plots_info("data", granularity = 3, n_experiments = 1)

# plot_multilevel <- function(
#   mult_plots_info,
#   ncol = NULL,                   # number of columns in layout
#   xlab = "Number of observations",
#   ylab = "Accuracy",
#   lwd = 2,
#   point_pch = 16,
#   legend_cex = 0.7
# ) {
#   stopifnot(is.list(mult_plots_info))
#
#   outer_names <- names(mult_plots_info)
#   n_plots <- length(outer_names)
#
#   # determine layout
#   if (is.null(ncol)) {
#     ncol <- ceiling(sqrt(n_plots))
#   }
#   nrow <- ceiling(n_plots / ncol)
#
#   # models are identical across plots -> get once
#   model_names <- names(mult_plots_info[[1]])
#   n_models <- length(model_names)
#   cols <- grDevices::rainbow(n_models)
#
#   # layout matrix (+1 for legend)
#   layout_mat <- matrix(seq_len(nrow * ncol), nrow, ncol, byrow = TRUE)
#   layout_mat <- rbind(layout_mat, rep(nrow * ncol + 1, ncol))
#
#   if (dev.cur() == 1) {
#   dev.new()
#   } else {
#   graphics.off()
#   dev.new()
#   }
#
#   layout(layout_mat, heights = c(rep(1, nrow), 0.25))
#
#   old_par <- par(no.readonly = TRUE)
#   # on.exit(par(old_par))
#
#   par(mar = c(4, 4, 3, 1) + 0.1)
#
#   # draw plots
#   for (outer_name in outer_names) {
#
#     inner_list <- mult_plots_info[[outer_name]]
#
#     ylim <- range(
#       unlist(lapply(inner_list, function(z) z[[1]])),
#       na.rm = TRUE
#     )
#
#     first <- inner_list[[1]]
#     plot(
#       first[[2]], first[[1]],
#       type = "b",
#       col = cols[1],
#       lwd = lwd,
#       pch = point_pch,
#       ylim = ylim,
#       xlab = xlab,
#       ylab = ylab,
#       main = outer_name
#     )
#
#     if (length(inner_list) > 1) {
#       for (i in 2:length(inner_list)) {
#         lines(
#           inner_list[[i]][[2]],
#           inner_list[[i]][[1]],
#           type = "b",
#           col = cols[i],
#           lwd = lwd,
#           pch = point_pch
#         )
#       }
#     }
#   }
#
#   # legend panel
#   par(mar = c(0, 0, 0, 0))
#   plot.new()
#   legend(
#     "center",
#     legend = model_names,
#     col = cols,
#     lwd = lwd,
#     pch = point_pch,
#     ncol = min(length(model_names), 4),
#     cex = legend_cex,
#     bty = "n"
#   )
# }

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
  )

  return(gg_plot)
}