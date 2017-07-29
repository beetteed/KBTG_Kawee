

Evaluation <- function(CrossValidationResult)
{
    Evaluate_result <-data.frame(
    dataType = c(),
    Sensitivity=c(),
    Specificity=c(),
    Precision = c(),
    Accuracy=c()
  )

   train<- CrossValidationResult[ which(CrossValidationResult$Type=="Train"),
                                  c("Classify" ,"Label")]
    
   test <- CrossValidationResult[ which(CrossValidationResult$Type=="Test"),
                                  c("Classify" ,"Label")]

  eval_Sen <- imSensitivity(train$Label,train$Classify)
  eval_Spec <- imSpecificity(train$Label,train$Classify)
  eval_Pre <- imPrecision(train$Label,train$Classify)
  eval_Acc <- imAccuracy(train$Label,train$Classify)

  Evaluate_result<-data.frame(
      dataType = "Train",
      Sensitivity=eval_Sen,
      Specificity=eval_Spec,
      Precision = eval_Pre,
      Accuracy=eval_Acc
  )
    
  eval_Sen <- imSensitivity(test$Label,test$Classify)
  eval_Spec <- imSpecificity(test$Label,test$Classify)
  eval_Pre <- imPrecision(test$Label,test$Classify)
  eval_Acc <- imAccuracy(test$Label,test$Classify)
    
  Evaluate_result<- rbind(Evaluate_result,data.frame(
      dataType = "Test",
      Sensitivity=eval_Sen,
      Specificity=eval_Spec,
      Precision = eval_Pre,
      Accuracy=eval_Acc
    ))


  
  names(Evaluate_result)<-c("dataType","Sensitivity","Specificity","Precision","Accuracy")
  
  Evaluate_result <- reshape(Evaluate_result, direction="long", 
                             varying=list(names(Evaluate_result)[2:5]), 
                             v.names="Evaluation_rate", 
                             idvar=c("dataType"),timevar="Evaluation_method",
                             times = c("Sensitivity","Specificity","Precision","Accuracy"))
  
  print(Evaluate_result)
  write.csv("Training_result.csv")

}


