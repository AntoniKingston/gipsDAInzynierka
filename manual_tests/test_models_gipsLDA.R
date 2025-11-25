# setwd("repo_inz/gipsDAInzynierka")
set.seed(42)

scenario_1_df = read.csv("data/scenario_1.csv")
train_indexes = sample(1:nrow(scenario_1_df), size = 0.7 * nrow(scenario_1_df))
train_data = scenario_1_df[train_indexes, ]
test_data = scenario_1_df[-train_indexes, ]

model_gipsLDA = gipslda(Y~., data = train_data)
print(model_gipsLDA)
plot(model_gipsLDA)

predictions_gipsLDA = predict(model_gipsLDA, newdata = test_data)
print(predictions_gipsLDA$class)
print(predictions_gipsLDA$posterior)
confusion_matrix_gipsLDA = table(test_data$Y, predictions_gipsLDA$class)
print(confusion_matrix_gipsLDA)
accuracy_gipsLDA = sum(diag(confusion_matrix_gipsLDA)) / sum(confusion_matrix_gipsLDA)
print(paste("Accuracy of gipsLDA:", round(accuracy_gipsLDA * 100, 2), "%"))

# compare to MASS::lda

model_MASS_lda = MASS::lda(Y~., data = train_data)
print(model_MASS_lda)
plot(model_MASS_lda)

predictions_MASS_lda = predict(model_MASS_lda, newdata = test_data)
print(predictions_MASS_lda$class)
print(predictions_MASS_lda$posterior)
confusion_matrix_MASS_lda = table(test_data$Y, predictions_MASS_lda$class)
print(confusion_matrix_MASS_lda)
accuracy_MASS_lda = sum(diag(confusion_matrix_MASS_lda)) / sum(confusion_matrix_MASS_lda)
print(paste("Accuracy of MASS::lda:", round(accuracy_MASS_lda * 100, 2), "%"))

