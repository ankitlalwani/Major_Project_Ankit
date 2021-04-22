Library(mlbench)
# feature selection using random forest
control2 <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
results <- rfe(patient[,1:32], patient[,33], sizes=c(1:32), rfeControl=control2) 

# summarize the results
print(results)

# list the chosen features
predictors(results) 

# plot the results 
plot(results)

# feature selection using Boruta
install.packages("Boruta")
library(Boruta)
patient_boruta <- Boruta(readmitted~., data = patient, doTrace = 2)
final_feature<- TentativeRoughFix(patient_boruta) 
getSelectedAttributes(final_feature, withTentative = F) 
# showes selected attributes


