# setwd("repo_inz/gipsDAInzynierka")
set.seed(42)

scenario_3_df = read.csv("data/scenario_3.csv")
train_indexes = sample(1:nrow(scenario_3_df), size = 0.7 * nrow(scenario_3_df))
train_data = scenario_3_df[train_indexes, ]
test_data = scenario_3_df[-train_indexes, ]

model_gipsQDA = gipsqda(Y~., data = train_data)

predictions_gipsQDA = predict(model_gipsQDA, newdata = test_data)
print(predictions_gipsQDA$class)
print(predictions_gipsQDA$posterior)
confusion_matrix_gipsQDA = table(test_data$Y, predictions_gipsQDA$class)
print(confusion_matrix_gipsQDA)
accuracy_gipsQDA = sum(diag(confusion_matrix_gipsQDA)) / sum(confusion_matrix_gipsQDA)
print(paste("Accuracy of gipsLDA:", round(accuracy_gipsQDA * 100, 2), "%"))

# compare to MASS::qda

model_MASS_qda = MASS::qda(Y~., data = train_data)

predictions_MASS_qda = predict(model_MASS_qda, newdata = test_data)
print(predictions_MASS_qda$class)
print(predictions_MASS_qda$posterior)
confusion_matrix_MASS_qda = table(test_data$Y, predictions_MASS_qda$class)
print(confusion_matrix_MASS_qda)
accuracy_MASS_qda = sum(diag(confusion_matrix_MASS_qda)) / sum(confusion_matrix_MASS_qda)
print(paste("Accuracy of MASS::qda:", round(accuracy_MASS_qda * 100, 2), "%"))
