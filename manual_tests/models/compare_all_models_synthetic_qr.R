#------------------------------------------------------------------------------
# Before running this script you have to create "plots/synthetic_data" directory
#-------------------------------------------------------------------------------

library(glue)
library(parallel)

set.seed(2137)
source("manual_tests/models/utils.R")

ps <- c("5","10")
ns <- c("2","5","10")
dists <- c("exp", "lnorm", "chisq")
perm_type_names <- c("sparse_trans", "dense_trans", "full_cycle")
map_bools <- c(TRUE, FALSE)

jobs <- expand.grid(
  p = ps,
  n = ns,
  dist = dists,
  perm_type_name = perm_type_names,
  MAP = map_bools,
  stringsAsFactors = FALSE
)

jobs$opt <- ifelse(as.numeric(jobs$p) < 10, "BF", "MH")

run_job <- function(p, n, dist, perm_type_name, MAP, opt) {

  message(
    "Running job: ",
    "p=", p, ", ",
    "n=", n, ", ",
    "dist=", dist, ", ",
    "perm_type=", perm_type_name, ", ",
    "MAP=", MAP, ", ",
    "opt=", opt
  )

  map_bool_chr <- as.character(MAP)

  filename <- glue("plots/synthetic_data/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.png")
  filename_plot_info <- glue("saved_objects/synthetic_data/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.rds")

  if (file.exists(filename)) {
    return(invisible(NULL))
  }

  plot_info <- generate_multiple_plots_info_qr(
    "data/synthetic_datasets",
    p = p,
    n = n,
    dist = dist,
    perm_type_name = perm_type_name,
    granularity = 13,
    lb = 16,
    n_experiments = 8,
    opt = opt,
    max_iter = 500,
    tr_ts_split = 0.7,
    MAP = MAP
  )

  saveRDS(plot_info, filename_plot_info)

  campaign_plot <- create_multilevel_plot(plot_info)

  ggplot2::ggsave(filename, plot = campaign_plot, width = 16, height = 9, dpi = 500)

  invisible(NULL)
}

mclapply(
  seq_len(nrow(jobs)),
  function(i) {
    with(jobs[i, ], run_job(p, n, dist, perm_type_name, MAP, opt))
  },
  mc.cores = parallel::detectCores() - 1
)

