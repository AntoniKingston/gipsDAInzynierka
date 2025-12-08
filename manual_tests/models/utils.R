library(gipsDA)
accuracy_experiment <- function(df, model, MAP = TRUE, opt = "BF", max_iter = 100, tr_ts_split = 0.7 ) {
  train_indexes = sample(1:nrow(df), size = tr_ts_split * nrow(df))
  train_data = df[train_indexes, ]
  test_data = df[-train_indexes, ]
  if (model == "lda") {
    classifier <- MASS::lda(Y ~ ., data = train_data, method = "mve")
  }
  else if (model == "qda") {
    classifier <- MASS::qda(Y ~ ., data = train_data, method = "mve")
  }
  else if (model == "gipsldacl") {
    classifier <- gipslda(Y ~ ., data = train_data, weighted_avg = FALSE, MAP = MAP, optimizer = opt, max_iter = max_iter)
  }
  else if (model == "gipsldawa") {
    classifier <- gipslda(Y ~ ., data = train_data, weighted_avg = TRUE, MAP = MAP, optimizer = opt, max_iter = max_iter)
  }
  else if (model == "gipsqda") {
    classifier <- gipsqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
  }
  else {
    classifier <- gipsmultqda(Y ~ ., data = train_data, MAP = MAP, optimizer = opt, max_iter = max_iter)
  }

  pred <- predict(classifier, test_data)$class
  y_true <- test_data$Y
  acc <- mean(pred == y_true)
  return(acc)

}
generate_accuracy_data <- function(df, model, granularity, lb, n_experiments, opt, max_iter, tr_ts_split) {
  if(nrow(df) < lb) {
  rlang::abort(c("x" = "dataset has less rows than specified lower bound"))
  }
  ns_obs <- round(exp(seq(log(lb), log(nrow(df)), length.out = granularity)))
  accs <- c()
  for (n_obs in ns_obs) {
    accs <- c(accs, mean(rep(accuracy_experiment(df[1:n_obs,], model), n_experiments)))
  }

  return(list("accs" = accs, "ns_obs" = ns_obs))
}

generate_single_plot_info <- function(scenario_data, model_names, granularity, lb, n_experiments, opt, max_iter, tr_ts_split) {
  plot_info <- lapply(model_names, function (x) generate_accuracy_data(scenario_data, x, granularity, lb, n_experiments, opt, max_iter, tr_ts_split))
  names(plot_info) <- model_names
  return(plot_info)
}

generate_multiple_plots_info <- function(data_path,
                             scenario_names = c("lda", "qda", "gipslda", "gipsqda", "gipsmultqda"),
                             model_names = c("lda", "qda", "gipsldacl", "gipsldawa", "gipsqda", "gipsmultqda"),
                             granularity = 25,
                             lb = 50,
                             n_experiments = 5,
                             opt = "BF",
                             max_iter = 100,
                             tr_ts_split = 0.7) {
  set.seed(42)
  data_paths <- paste(data_path, glue::glue("/scenario_{scenario_names}.csv"), sep = "")
  #read in with shuffling
  datasets <- lapply(lapply(data_paths, read.csv), function(x) x[sample(nrow(x)),])
  multiple_plots_info <- lapply(datasets, function(x) generate_single_plot_info(x, model_names, granularity, lb, n_experiments, opt, max_iter, tr_ts_split))
  names(multiple_plots_info) <- scenario_names
  return(multiple_plots_info)
}
## przykładowe wywołanie (działa jak się dobrze dane wygeneruje)
mult_plots_info <- generate_multiple_plots_info("data", granularity = 3, n_experiments = 1)

plot_multilevel <- function(
  mult_plots_info,
  ncol = NULL,                   # number of columns in layout
  xlab = "Number of observations",
  ylab = "Accuracy",
  lwd = 2,
  point_pch = 16,
  legend_cex = 0.7
) {
  stopifnot(is.list(mult_plots_info))

  outer_names <- names(mult_plots_info)
  n_plots <- length(outer_names)

  # determine layout
  if (is.null(ncol)) {
    ncol <- ceiling(sqrt(n_plots))
  }
  nrow <- ceiling(n_plots / ncol)

  # models are identical across plots -> get once
  model_names <- names(mult_plots_info[[1]])
  n_models <- length(model_names)
  cols <- grDevices::rainbow(n_models)

  # layout matrix (+1 for legend)
  layout_mat <- matrix(seq_len(nrow * ncol), nrow, ncol, byrow = TRUE)
  layout_mat <- rbind(layout_mat, rep(nrow * ncol + 1, ncol))

  layout(layout_mat, heights = c(rep(1, nrow), 0.25))

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(4, 4, 3, 1) + 0.1)

  # draw plots
  for (outer_name in outer_names) {

    inner_list <- mult_plots_info[[outer_name]]

    ylim <- range(
      unlist(lapply(inner_list, function(z) z[[1]])),
      na.rm = TRUE
    )

    first <- inner_list[[1]]
    plot(
      first[[2]], first[[1]],
      type = "b",
      col = cols[1],
      lwd = lwd,
      pch = point_pch,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      main = outer_name
    )

    if (length(inner_list) > 1) {
      for (i in 2:length(inner_list)) {
        lines(
          inner_list[[i]][[2]],
          inner_list[[i]][[1]],
          type = "b",
          col = cols[i],
          lwd = lwd,
          pch = point_pch
        )
      }
    }
  }

  # legend panel
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(
    "center",
    legend = model_names,
    col = cols,
    lwd = lwd,
    pch = point_pch,
    ncol = min(length(model_names), 4),
    cex = legend_cex,
    bty = "n"
  )
}