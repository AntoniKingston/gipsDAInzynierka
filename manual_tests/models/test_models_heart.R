set.seed(2137)
source("manual_tests/models/utils.R")
df <- read.csv("data/real_world_datasets/heart.csv")
colnames(df)[colnames(df) == "HeartDisease"] <- "Y"
df_encoded <- fastDummies::dummy_cols(df, select_columns = c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope"), remove_selected_columns = TRUE)[sample(1:nrow(df)),]
y <- df_encoded$Y
pca_obj <- prcomp(subset(df_encoded, select = -c(Y)), scale. = TRUE)
pca_frame <- data.frame(pca_obj$x)
pca_frame <- pca_frame[,1:10]
pca_frame$Y <- y
heart_plot_data <- generate_single_plot_info(pca_frame, granularity = 25, lb = 200,  n_experiments = 5, opt = "MH", max_iter = 100, tr_ts_split = 0.7, MAP = TRUE)
real_plot_data = list("heart" = heart_plot_data)
create_multilevel_plot(real_plot_data)
pca_frame
heart_plot_data
