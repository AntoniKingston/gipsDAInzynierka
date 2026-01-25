set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing sensorless dataset...")
df <- read.table("data/real_world_datasets/Sensorless_drive_diagnosis.txt", header = FALSE)

colnames(df)[colnames(df) == "V49"] <- "Y"

df <- fix_tiny_values(df)
df <- remove_collinear_features(df)
df2 <- df[sample(1:nrow(df)), ]

exp_data <- generate_single_plot_info_real_data(df2,
                                                granularity = 16, lb = 20, up=4000,
                                                n_experiments = 30, opt = "MH",
                                                max_iter = 1500, tr_ts_split = 0.7, MAP = TRUE)

plot_data <- exp_data$plot_info
test_data <- exp_data$test_info
real_plot_data = list("sensorless" = plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/sensorless_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/sensorless_plot_data.rds")
saveRDS(test_data, "data/real_world_datasets/sensorless_test_data.rds")
