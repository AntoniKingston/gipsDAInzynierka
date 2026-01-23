set.seed(2137)
source("manual_tests/models/utils.R")
print("Processing credit dataset...")

df <- read.table("data/real_world_datasets/creditcard.csv", sep = ",", header = TRUE)
colnames(df)[colnames(df) == "Class"] <- "Y"
df$Y <- as.factor(df$Y)

table_y <- table(df$Y)
min_class <- names(which.min(table_y))
maj_class <- names(which.max(table_y))
n_min <- min(table_y)

target_minority_share <- 0.40
n_maj_to_sample <- round(n_min * ((1 - target_minority_share) / target_minority_share))

message(sprintf("Target balance: Majority ~60%% (%d), Minority ~40%% (%d).",
                n_maj_to_sample, n_min))

df_minority <- df[df$Y == min_class, ]
df_majority <- df[df$Y == maj_class, ]
df_majority_downsampled <- df_majority[sample(nrow(df_majority), n_maj_to_sample), ]

df <- rbind(df_majority_downsampled, df_minority)
df <- df[sample(nrow(df)), ]

message(sprintf("New dataset size: %d rows.", nrow(df)))

df <- remove_low_variance_columns(df, threshold = 0.8)
df <- remove_collinear_features(df, cutoff=0.90)
df <- fix_tiny_values(df)
df = clean_infinite_values(df)
df <- df[sample(1:nrow(df)), ]

credit_plot_data <- generate_single_plot_info_real_data(df,
                                                granularity = 20, lb = 20,
                                                n_experiments = 30, opt = "MH",
                                                max_iter = 1000, tr_ts_split = 0.7, MAP = TRUE)

real_plot_data = list("creditcard" = credit_plot_data)
plot_object <- create_multilevel_plot(real_plot_data)
ggsave("data/real_world_datasets/creditcard_plot.png",
       plot = plot_object, width = 10, height = 5, dpi = 300)

saveRDS(real_plot_data, "data/real_world_datasets/creditcard_plot_data.rds")
