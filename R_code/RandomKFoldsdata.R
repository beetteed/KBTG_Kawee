randomKFoldsdata <- function(df,times,numfolds)
{
  set.seed(Sys.time())
  KFoldsdata<-list()

  
  foldsData <-data.frame(Id=c(),times=c(),folds=c(),type=c())
  
  for(t in 1:times)
  {
    ## Randomly saperated to train and test
    randomData <- sample(df)
    bound <- floor(length(df)/numfolds)
    nrFolds = numfolds
    # Generate array containing fold-number for each sample (row)
    folds <- rep_len(1:nrFolds, length(randomData))
    for(k in 1:numfolds)
    {
      fold <- which(folds == k )
      
      dataTest <- as.data.frame(randomData[fold])
      dataTrain <- as.data.frame(randomData[-fold])
      
      names(dataTest) <-c("Id")
      names(dataTrain) <-c("Id")

      # Collect and save folds information as csv
      foldsData<-rbind(foldsData,data.frame(
        Id = dataTest$Id,times=t,folds=k,type = "test"))
      foldsData<-rbind(foldsData,data.frame(
        Id = dataTrain$Id,times=t,folds=k,type = "train"))


      # Assisn value to global environment
      assign(paste("dataTest_T",t,"_F",k,sep = ""), dataTest,envir = .GlobalEnv)
      assign(paste("dataTrain_T",t,"_F",k, sep=""), dataTrain,envir = .GlobalEnv)
      
 
    }
    
  }
  write.csv(foldsData, file = paste("R_code/Result/foldsDatas.csv",sep=""))
}


randomKFoldsdata(df_train$card_no,1,10)
