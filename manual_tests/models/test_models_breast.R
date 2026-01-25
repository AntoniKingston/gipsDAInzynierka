set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing breast dataset...")
col_names <- glue::glue("feature_{1:30}")
col_names <- c("Id", "Y", col_names)
df <- read.csv("data/real_world_datasets/breast.data",
               header = FALSE,
               col.names = col_names,
               stringsAsFactors = FALSE)[,2:32]
breast_exp_data <- generate_single_plot_info_real_data(
  df, granularity = 16, lb = 20, up=150, n_experiments = 30,
  opt = "MH", max_iter = 1500, tr_ts_split = 0.7, MAP = TRUE)

breast_plot_data <- breast_exp_data$plot_info
breast_test_data <- breast_exp_data$test_info
real_plot_data <- list("breast" = breast_plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/breast_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/breast_plot_data.rds")
saveRDS(breast_test_data, "data/real_world_datasets/breast_test_data.rds")
