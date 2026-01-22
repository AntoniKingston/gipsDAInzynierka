set.seed(2137)
source("manual_tests/models/utils.R")
df <- read.csv("data/real_world_datasets/heart.csv")
colnames(df)[colnames(df) == "HeartDisease"] <- "Y"
df_encoded <- fastDummies::dummy_cols(df, select_columns = c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope"), remove_selected_columns = TRUE, remove_first_dummy = TRUE)[sample(1:nrow(df)),]
y <- df_encoded$Y
heart_plot_data <- generate_single_plot_info_real_data(df_encoded, granularity = 20, lb = 16,
                                             n_experiments = 30, opt = "MH", max_iter = 1000,
                                             tr_ts_split = 0.7, MAP = TRUE)
real_plot_data = list("heart" = heart_plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/heart_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/heart_plot_data.rds")
