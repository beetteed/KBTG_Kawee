
library(neuralnet)



CrossValidationResult <- data.frame(
  Time =c(),
  Fold =c(),
  Type=c(),
  Predict=c(),
  Label =c()
)
for(t in 1:1)
{
  
  for(k in 1:1)
  {
    fold<-paste("T",t,"_F",k,sep="")
    dataTest_Id <- get(paste("dataTest_",fold,sep = ""))
    dataTrain_Id <- get(paste("dataTrain_",fold, sep=""))
    
    selectedAtrr <- c("card_no","bill_cyc","age","bill_cyc","incm_amt","cr_lmt_amt","prev_cr_lmt_amt")
    
    dataTest <- df_train[ which(df_train$card_no %in% dataTest_Id$Id),selectedAtrr]
    dataTrain <- df_train[ which(df_train$card_no %in% dataTrain_Id$Id),selectedAtrr ]
    
    
    form.in <- as.formula(paste(selectedAtrr[1],"~", paste(selectedAtrr[!selectedAtrr %in% selectedAtrr[1]], 
                                                       collapse = " + ")))
    # Scale data ignore label
    dataTest[, -c(1)] <- scale(dataTest[, -c(1)])
    dataTrain[, -c(1)] <- scale(dataTrain[, -c(1)])
    
    struct<-c(5,3)
    Model<-neuralnet(form.in,data=dataTrain,hidden=struct,
                     algorithm = "rprop+", err.fct = "sse", act.fct = "logistic", 
                     threshold = 0.1,learningrate =0.5,rep=1000,
                     linear.output=TRUE)
    
    # SaveModel <-paste("T",t,"_F",k,sep="")
    Modelname <- paste("NN_Model_",fold,".Rdata",sep="")
    save(Model,file=Modelname)
    
    #plot Model 
    jpeg(file=paste("NNmodel_",fold,".jpg",sep=""),
         units="in", width=5, height=5,res=300)
    p<-plot(Model)
    print(p)
    dev.off()
    
    # Compute predictions
    pr.train <- compute(Model, dataTest[selectedAtrr[2:length(selectedAtrr)]])
    # Extract results
    res.train<- pr.test$net.result
    
    pr.test <- compute(Model, dataTest[selectedAtrr[2:length(selectedAtrr)]])
    # Extract results
    res.test<- pr.test$net.result
    
    #save result for eval
    CrossValidationResult <- rbind(CrossValidationResult,
                                   data.frame(
                                     Time =t,
                                     Fold =k,
                                     Type="Train",
                                     Predict=res.train,
                                     Label =dataTrain$label
                                   ))
    
    CrossValidationResult <- rbind(CrossValidationResult,
                                   data.frame(
                                     Time =t,
                                     Fold =k,
                                     Type="Test",
                                     Predict=res.test,
                                     Label =dataTest$label
                                   ))

  }
  
}

