set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing heart dataset...")
df <- read.csv("data/real_world_datasets/heart.csv")
colnames(df)[colnames(df) == "HeartDisease"] <- "Y"
df_encoded <- fastDummies::dummy_cols(df, select_columns = c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope"),
                                      remove_selected_columns = TRUE, remove_first_dummy = TRUE)[sample(1:nrow(df)),]

df_encoded = remove_low_variance_columns(df_encoded, threshold = 0.9)

heart_exp_data <- generate_single_plot_info_real_data(df_encoded, granularity = 16, lb = 20, up=350,
                                             n_experiments = 30, opt = "MH", max_iter = 1500,
                                             tr_ts_split = 0.7, MAP = TRUE)

heart_plot_data <- heart_exp_data$plot_info
heart_test_data <- heart_exp_data$test_info
real_plot_data = list("heart" = heart_plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/heart_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/heart_plot_data.rds")
saveRDS(heart_test_data, "data/real_world_datasets/heart_test_data.rds")
