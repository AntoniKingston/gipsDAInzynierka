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




