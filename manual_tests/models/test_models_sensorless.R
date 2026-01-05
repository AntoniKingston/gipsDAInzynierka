set.seed(2137)
source("manual_tests/models/utils.R")
df <- read.table("data/real_world_datasets/Sensorless_drive_diagnosis.txt", header = FALSE)

colnames(df)[colnames(df) == "V49"] <- "Y"
df2 <- df[sample(1:nrow(df)),]

plot_data <- generate_single_plot_info(df2,
                                                granularity = 13, lb = 16,
                                                n_experiments = 8, opt = "MH",
                                                max_iter = 500, tr_ts_split = 0.7, MAP = TRUE)

real_plot_data = list("sensorless" = plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/sensorless_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)