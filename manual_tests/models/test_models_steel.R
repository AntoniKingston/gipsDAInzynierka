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

steel_plot_data <- generate_single_plot_info_real_data(df,
                                                granularity = 20, lb = 20,
                                                n_experiments = 30, opt = "MH",
                                                max_iter = 1000, tr_ts_split = 0.7, MAP = TRUE)

real_plot_data = list("steel" = steel_plot_data)
plot_object <- create_multilevel_plot(real_plot_data)

ggsave("data/real_world_datasets/steel_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/steel_plot_data.rds")
