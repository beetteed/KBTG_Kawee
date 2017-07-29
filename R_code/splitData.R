randomKFoldsdata <- function(df)
{
  set.seed(Sys.time())
  KFoldsdata<-list()

  foldsData <-data.frame(Id=c(),times=c(),folds=c(),type=c())
  

    ## Randomly saperated to train and test
    randomData <- sample(df)
    bound <- floor(as.numeric(nrow(df))/10)
    # Generate array containing fold-number for each sample (row)
    dataTest <- as.data.frame(randomData[c(1:(3*bound)),])
    dataTrain <- as.data.frame(randomData[-c(1:(3*bound)),])

    # Assisn value to global environment
    assign(paste("dataTest",sep = ""), dataTest,envir = .GlobalEnv)
    assign(paste("dataTrain", sep=""), dataTrain,envir = .GlobalEnv)

}

randomKFoldsdata(df_train)
