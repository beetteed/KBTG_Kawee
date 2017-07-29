library(e1071)

CrossValidationResult <- data.frame(
  Time =c(),
  Fold =c(),
  Type=c(),
  Predict=c(),
  Label =c()
)

numfolds<-10
for(t in 1:1)
{
  
  for(k in 1:numfolds)
  {
    fold<-paste("T",t,"_F",k,sep="")
    dataTest_Id <- get(paste("dataTest_",fold,sep = ""))
    dataTrain_Id <- get(paste("dataTrain_",fold, sep=""))
    
    selectedAtrr <- c("card_no","label","bill_cyc","age","bill_cyc","incm_amt","cr_lmt_amt","prev_cr_lmt_amt")
    
    dataTest <- df_train[ which(df_train$card_no %in% dataTest_Id$Id),selectedAtrr]
    dataTrain <- df_train[ which(df_train$card_no %in% dataTrain_Id$Id),selectedAtrr ]
    
    
    form.in <- as.formula(paste(selectedAtrr[2],"~", paste(selectedAtrr[!selectedAtrr %in% selectedAtrr[1:2]], 
                                                       collapse = " + ")))

    Model<-svm(form.in,data=dataTrain)
    

    # Compute predictions
    res.train <- predict(Model, dataTest[selectedAtrr[2:length(selectedAtrr)]])
    res.train.class <- apply(as.data.frame(res.train), 1, cut,c(-Inf,0.5, Inf), labels=c(0,1))
    
    
    res.test <- predict(Model, dataTest[selectedAtrr[2:length(selectedAtrr)]])
    res.test.class <- apply(as.data.frame(res.test), 1, cut,c(-Inf,0.5, Inf), labels=c(0,1))
   
    #save result for eval
    CrossValidationResult <- rbind(CrossValidationResult,
                                   data.frame(
                                     Time =t,
                                     Fold =k,
                                     Type="Train",
                                     Predict=res.train,
                                     Classify = res.train.class,
                                     Label =dataTrain$label
                                   ))
    
    CrossValidationResult <- rbind(CrossValidationResult,
                                   data.frame(
                                     Time =t,
                                     Fold =k,
                                     Type="Test",
                                     Predict=res.test,
                                     Classify = res.train.class,
                                     Label =dataTest$label
                                   ))

  }
  
}

