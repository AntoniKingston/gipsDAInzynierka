path <- "saved_objects/tests"

files <- list.files(path, pattern = "\\.rds$", full.names = TRUE)

data_list <- lapply(files, readRDS)

scenario_names <- c("qda", "gipsqda", "gipsmultqda", "lda", "gipslda")

threshold <- 0.05

truncated_data_list <- lapply(data_list, function(data_obj) {
  lapply(data_obj, function(scenario){
  scenario[scenario < threshold]
  })
})

# print scenarios with p-values below threshold
for (i in seq_along(truncated_data_list)) {
    cat("File:", files[i], "\n")
    for (scenario_name in scenario_names) {
        p_values <- truncated_data_list[[i]][[scenario_name]]
        if (length(p_values) > 0) {
            cat("  Scenario:", scenario_name, "\n")
            print(p_values)
        }
    }
    cat("\n")
}
# count p-values below threshold for each scenario and each file
count_below_threshold <- lapply(truncated_data_list, function(data_obj) {
    sapply(data_obj, length)
})
names(count_below_threshold) <- files
print(count_below_threshold)
