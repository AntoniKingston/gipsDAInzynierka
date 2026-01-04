#------------------------------------------------------------------------------
# Before running this script you have to create "plots/synthetic_data" directory
#-------------------------------------------------------------------------------
set.seed(2137)
source("manual_tests/models/utils.R")
ps <- c("5","10")
ns <- c("2","5","10")
dists <- c("exp", "lnorm", "chisq")
perm_type_names <- c("sparse_trans", "dense_trans", "full_cycle")
map_bools <- c("TRUE", "FALSE")
for (p in ps) {
  if (as.numeric(p) < 10) {
    opt <- "BF"
  } else {
    opt <- "MH"
  }
  for (n in ns) {
    for (dist in dists) {
      for (perm_type_name in perm_type_names) {
        for (map_bool in map_bools) {
          map_bool_chr <- as.character(map_bool)
          filename <- glue::glue("plots/synthetic_data/synth_{p}_{n}_{dist}_{perm_type_name}_{map_bool_chr}.png")

          if (!file.exists(filename)) {
            plot_info <- generate_multiple_plots_info_qr("data/synthetic_datasets",
                                                     p,
                                                     n,
                                                     dist,
                                                     perm_type_name,
                                                     granularity = 5,
                                                     lb=16,
                                                     n_experiments = 5,
                                                     opt = opt,
                                                     max_iter = 100,
                                                     tr_ts_split = 0.7,
                                                     MAP = map_bool)
            campaign_plot <- create_multilevel_plot(plot_info)
            ggplot2::ggsave(filename, plot = campaign_plot, width = 16, height = 9, dpi = 500)


          }
        }
      }
    }
  }

}
