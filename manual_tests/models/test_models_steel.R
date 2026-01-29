set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing steel dataset...")

df <- read.table("data/real_world_datasets/Faults.NNA")
cols <- read.table("data/real_world_datasets/Faults27x7_var")
colnames(df) <- cols$V1

class_cols <- names(df)[(ncol(df)-6):ncol(df)]
df$Y <- max.col(df[, class_cols], ties.method = "first") - 1
df <- df[, !(names(df) %in% class_cols)]
df$Y <- as.factor(df$Y)

df <- remove_low_variance_columns(df, threshold = 0.8)
df <- remove_collinear_features(df, cutoff=0.90)
df <- fix_tiny_values(df)
df = clean_infinite_values(df)
df <- df[sample(1:nrow(df)), ]

spe_idx = c(1,4,8)

steel_exp_data <- generate_single_plot_info_real_data(df,
                                                granularity = 16, lb = 20, up=800,
                                                n_experiments = 30, n_experiments_spe = 100, spe_idx = spe_idx, opt = "MH",
                                                max_iter = 1500, tr_ts_split = 0.7, MAP = TRUE)

steel_plot_data <- steel_exp_data$plot_info
steel_test_data <- steel_exp_data$test_info
real_plot_data = list("steel" = steel_plot_data)
plot_object <- create_multilevel_plot(real_plot_data)
plot_object_auc <- create_multilevel_plot(real_plot_data, "vucs", ylab = "AUC")
boxplot_object <- create_multilevel_boxplot(real_plot_data, spe_idx)

ggsave("data/real_world_datasets/steel_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)
ggsave("data/real_world_datasets/steel_plot_auc.png",
       plot = plot_object_auc, width = 10, height = 5, dpi = 300)
ggsave("data/real_world_datasets/steel_boxplot.png",
       plot = boxplot_object, width = 6, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/steel_plot_data.rds")
saveRDS(steel_test_data, "data/real_world_datasets/steel_test_data.rds")
