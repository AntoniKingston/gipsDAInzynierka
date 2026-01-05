set.seed(2137)
source("manual_tests/models/utils.R")
df <- read.csv("data/real_world_datasets/emotions.csv")

has_packages <- requireNamespace("randomForest", quietly = TRUE) &&
                requireNamespace("dplyr", quietly = TRUE)

colnames(df)[colnames(df) == "label"] <- "Y"

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

emotions_plot_data <- generate_single_plot_info(df_final,
                                                granularity = 13, lb = 16,
                                                n_experiments = 8, opt = "MH",
                                                max_iter = 500, tr_ts_split = 0.7, MAP = TRUE)

real_plot_data = list("emotions" = emotions_plot_data)

plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/emotions_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)