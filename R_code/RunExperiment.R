source('R_code/init.R')
source('R_code/LoadData.R')


splitData(df_train)
source('R_code/SVMTrainModel.R')
Evaluation(CrossValidationResult)