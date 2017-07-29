# load R function
source('../R/Graphcut/getImageData.R')
library(parallel)
Evaluation <- function(
  predict,
  result,
  times,
  numfolds

)
{

  source('R_code/Evaluation/imAccuracy.R')
  source('R_code/Evaluation/imPrecision.R')
  source('R_code/Evaluation/imSensitivity.R')
  source('R_code/Evaluation/imSpecificity.R')
  source('R_code/Evaluation/summarySE.R')

  Evaluate_result <-data.frame(time = c(),
                               fold = c(),
                               dataType = c(),
                               Sensitivity=c(),
                               Specificity=c(),
                               Precision = c(),
                               Accuracy=c()
  
                               
  )

  EvaluationList <- 1:10
  # run the cluster
  # initiate 3 cluster
  clus <- makeCluster(3)
  # export img and functions to cluster 
  clusterExport(clus,c("CrossValidationResult",
                       "Evaluate_result","imSensitivity","imSpecificity",
                       "imPrecision","imAccuracy"),envir=environment())
  clusterEvalQ(clus, library(reshape2))

  # get all EvaluationList
  Evaluation_all <- lapply(EvaluationList,function(i){

   train<- CrossValidationResult[ which(CrossValidationResult$Fold==i &
           CrossValidationResult$Type=="Train"),c("Classify" ,"Label")]
    
   test <- CrossValidationResult[ which(CrossValidationResult$Fold==i &
           CrossValidationResult$Type=="Test"),c("Classify" ,"Label")]
    
    eval_Sen <- imSensitivity(train$Label,train$Classify)
    eval_Spec <- imSpecificity(train$Label,train$Classify)
    eval_Pre <- imPrecision(train$Label,train$Classify)
    eval_Acc <- imAccuracy(train$Label,train$Classify)

    Evaluation_all<-data.frame(
      fold = i,
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
    
    Evaluation_all<- rbind(Evaluation_all,data.frame(
      fold = i,
      dataType = "Test",
      Sensitivity=eval_Sen,
      Specificity=eval_Spec,
      Precision = eval_Pre,
      Accuracy=eval_Acc
    ))
    return(Evaluation_all)
  })
  
  stopCluster(clus)
  gc()
  
  # get all Evaluation
  Evaluate_result<-bind_rows(Evaluation_all) 
  
  
  names(Evaluate_result)<-c("fold","dataType","Sensitivity","Specificity","Precision","Accuracy")
  
  Evaluate_result <- reshape(Evaluate_result, direction="long", varying=list(names(Evaluate_result)[3:6]), 
                             v.names="Evaluation_rate", 
                             idvar=c("fold","dataType"),timevar="Evaluation_method",
                             times = c("Sensitivity","Specificity","Precision","Accuracy"))

  
  jpeg(file=paste("summary_result.jpg",sep=""),
       units="in", width=8, height=11,res=300)
  p<-ggplot(Evaluate_result, 
            aes(y=Evaluation_rate,x=fold,group=dataType,colour = dataType)) +
    geom_point(size=1)+
    geom_line()+

    facet_grid(Evaluation_method~.) +
    theme(strip.text.x = element_text(size = 14))+
    theme(strip.text.y = element_text(size = 14))+
    theme(text = element_text(size=14))
  print(p)
  dev.off()
  

  
  
  
  }

