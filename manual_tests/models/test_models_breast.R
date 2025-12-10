set.seed(2137)
source("manual_tests/models/utils.R")
col_names <- glue::glue("feature_{1:30}")
col_names <- c("Id", "Y", col_names)
df <- read.csv("data/breast.data",
               header = FALSE,
               col.names = col_names,
               stringsAsFactors = FALSE)[,2:32]
breast_plot_data <- generate_single_plot_info(df, granularity = 25, lb = 15,  n_experiments = 5, opt = "MH", max_iter = 100, tr_ts_split = 0.7, MAP = TRUE)
real_plot_data <- list("breast" = breast_plot_data)
