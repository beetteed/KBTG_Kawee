

CrossValidationResult <- data.frame(
  Type=c(),
  Predict=c(),
  Label =c()
)


selectedAtrr <- c("label","bill_cyc","age","bill_cyc","incm_amt","cr_lmt_amt","prev_cr_lmt_amt")
    
dataTest <- df_train[,selectedAtrr]
dataTrain <- df_train[,selectedAtrr ]
    
    
form.in <- as.formula(paste(selectedAtrr[1],"~", 
                      paste(selectedAtrr[!selectedAtrr %in% selectedAtrr[1]], 
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
                                     Type="Train",
                                     Predict=res.train,
                                     Classify = res.train.class,
                                     Label =dataTrain$label
                                   ))
CrossValidationResult <- rbind(CrossValidationResult,
                                   data.frame(
                                     Type="Test",
                                     Predict=res.test,
                                     Classify = res.train.class,
                                     Label =dataTest$label
                                   ))

