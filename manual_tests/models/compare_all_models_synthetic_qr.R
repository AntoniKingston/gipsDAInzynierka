#------------------------------------------------------------------------------
# Before running this script you have to create "plots/synthetic_data_plot_info" directory
#-------------------------------------------------------------------------------

library(glue)
library(parallel)

set.seed(2137)
source("manual_tests/models/utils.R")

ps <- c("5","10")
# ps <- c("5")
ns <- c("2","5","10")
# ns <- c("2")
perm_lists_prop <- list(
  # For p=5 (5 dimensions)
  "5" = list(
    "sparse_trans" = "(1,2)",
    "dense_trans" = "(1,2)(3,4)",
    "full_cycle" = "(1,2,3,4,5)",
    "perm_4" = "(1,3)(2,4)",
    "perm_5" = "(1,2,3)(4,5)",
    "perm_6" = "(1,4)(2,5)",
    "perm_7" = "(1,2,3,4)",
    "perm_8" = "(1,5)(2,3)",
    "perm_9" = "(1,2)(3,5)",
    "perm_10" = "(1,3,5)(2,4)"
  ),
  # For p=10 (10 dimensions)
  "10" = list(
    "sparse_trans" = "(1,2)(5,6)(8,9)",
    "dense_trans" = "(1,2)(3,4)(5,6)(7,8)(9,10)",
    "full_cycle" = "(1,2,3,4,5,6,7,8,9,10)",
    "perm_4" = "(1,3)(2,4)(7,9)",
    "perm_5" = "(1,2,3,4)(5,6,7)",
    "perm_6" = "(1,5)(2,6)(3,7)(4,8)",
    "perm_7" = "(1,2,3,4,5,6)",
    "perm_8" = "(1,10)(2,9)(3,8)(4,7)",
    "perm_9" = "(1,2)(3,4)(5,6)(7,8)",
    "perm_10" = "(1,3,5,7,9)(2,4,6,8,10)"
  )
)
dists <- c("exp", "lnorm", "chisq")
map_bools <- c(TRUE, FALSE)
perm_type_names <- c("sparse_trans", "dense_trans", "full_cycle")

jobs <- expand.grid(
  p = ps,
  n = ns,
  dist = dists,
  MAP = map_bools,
  perm_type_name = perm_type_names,
  stringsAsFactors = FALSE
)

jobs$opt <- ifelse(as.numeric(jobs$p) < 10, "BF", "MH")

run_job <- function(p, n, dist, perm_type_name, MAP, opt) {
  perms <- perm_lists_prop[[p]]
  if (perm_type_name == "dense_trans") {
    perms[c(1,2)] <- perms[c(2,1)]
  } else if (perm_type_name == "full_cycle") {
    perms[c(1,3)] <- perms[c(3,1)]
  }

  map_bool_chr <- as.character(MAP)

  filename <- glue("plots/synthetic_data/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.png")
  filename_auc <- glue("plots/synthetic_data/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}_auc.png")
  filename_boxplot <- glue("plots/synthetic_data/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}_spe.png")
  filename_plot_info <- glue("saved_objects/synthetic_data_plot_info/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.rds")
  filename_scenarios_metadata <- glue("saved_objects/synthetic_data_matrices_and_means/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.rds")
  filename_tests <- glue("saved_objects/tests/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.rds")

  if (file.exists(filename)) {
    return(invisible(NULL))
  }

  message(
    "Running job: ",
    "p=", p, ", ",
    "n=", n, ", ",
    "dist=", dist, ", ",
    "perm_type=", perm_type_name, ", ",
    "MAP=", MAP, ", ",
    "opt=", opt
  )

  spe_indexes <- c(1,3,5)

  scenario_info <- generate_multiple_plots_info_qr(p = as.numeric(p),
    n_classes = as.integer(n),
    perms = perms,
    lambda_dist = dist,
    granularity = 10,
    lb = 16,
    n_experiments = 30,
    n_experiments_spe = 100,
    spe_idx = spe_indexes,
    opt = opt,
    max_iter = 1000,
    n_test = 1000,
    MAP = MAP
  )

  plot_info <- scenario_info[["plot"]]
  meta_info <- scenario_info[["meta"]]
  test_info <- scenario_info[["test"]]

  saveRDS(plot_info, filename_plot_info)
  saveRDS(meta_info, filename_scenarios_metadata)
  saveRDS(test_info, filename_tests)

  campaign_plot <- create_multilevel_plot(plot_info)
  campagn_plot_auc <- create_multilevel_plot(plot_info, "vucs", ylab = "AUC")
  campaign_boxplot <- create_multilevel_boxplot(plot_info, spe_indexes)

  ggplot2::ggsave(filename, plot = campaign_plot, width = 16, height = 9, dpi = 500)
  ggplot2::ggsave(filename_auc, plot = campagn_plot_auc, width = 16, height = 9, dpi = 500)
  ggplot2::ggsave(filename_boxplot, plot = campaign_boxplot, width = 16, height = 9, dpi = 500)

  invisible(NULL)
}

mclapply(
  seq_len(nrow(jobs)),
  function(i) {
    with(jobs[i, ], run_job(p, n, dist, perm_type_name, MAP, opt))
  },
  mc.cores = parallel::detectCores() - 1
)





