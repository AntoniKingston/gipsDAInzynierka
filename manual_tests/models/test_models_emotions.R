set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing emotions dataset...")
df <- read.csv("data/real_world_datasets/emotions.csv")

has_packages <- requireNamespace("randomForest", quietly = TRUE) &&
                requireNamespace("dplyr", quietly = TRUE)

colnames(df)[colnames(df) == "label"] <- "Y"

df = remove_low_variance_columns(df, threshold = 0.9)

# take only first 100 columns for faster testing
if (!has_packages) {
  # df_small <- df[, 1:101]
  stop("Please install the 'randomForest' and 'dplyr' packages to run this code.")
}
df_base <- data.frame(df)
df_base$Y <- as.factor(df_base$Y)

rf_model <- randomForest::randomForest(Y ~ ., data = df_base,
                                       importance = TRUE, ntree = 100)

importances <- randomForest::importance(rf_model, type = 2)

imp_df <- data.frame(
  Feature = rownames(importances),
  Importance = importances[, 1]
)
imp_sorted <- dplyr::arrange(imp_df, dplyr::desc(Importance))
imp_top30 <- head(imp_sorted, 30)
top_30_features <- dplyr::pull(imp_top30, Feature)
df_final <- df[, c(top_30_features, "Y")]

df_final <- df_final[sample(1:nrow(df_final)),]

emotions_exp_data <- generate_single_plot_info_real_data(df_final,
                                                granularity = 16, lb = 20, up=500,
                                                n_experiments = 30, opt = "MH",
                                                max_iter = 1500, tr_ts_split = 0.7, MAP = TRUE)

emotions_plot_data <- emotions_exp_data$plot_info
emotions_test_data <- emotions_exp_data$test_info
real_plot_data = list("emotions" = emotions_plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/emotions_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/emotions_plot_data.rds")
saveRDS(emotions_test_data, "data/real_world_datasets/emotions_test_data.rds")
