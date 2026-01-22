set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing breast dataset...")
col_names <- glue::glue("feature_{1:30}")
col_names <- c("Id", "Y", col_names)
df <- read.csv("data/real_world_datasets/breast.data",
               header = FALSE,
               col.names = col_names,
               stringsAsFactors = FALSE)[,2:32]
breast_plot_data <- generate_single_plot_info_real_data(
  df, granularity = 20, lb = 20,  n_experiments = 30, opt = "MH",
  max_iter = 1000, tr_ts_split = 0.7, MAP = TRUE)
real_plot_data <- list("breast" = breast_plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/breast_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/breast_plot_data.rds")
