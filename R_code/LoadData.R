
library(dplyr)
datapath <-"D:/tj_datatrack_problem/"
result <-"D:/tj_datatrack_problem/Result"

df_train<- read.csv(paste(datapath,"1/tj_01_training.csv",sep=""),header = FALSE)
df_test<- read.csv(paste(datapath,"1/tj_01_test.csv",sep=""),header = FALSE)


df1<- read.csv(paste(datapath,"/1/tj_01_creditcard_card.csv",sep=""))
df2<- read.csv(paste(datapath,"/1/tj_01_creditcard_customer.csv",sep=""))
df3<- read.csv(paste(datapath,"/1/tj_01_creditcard_transaction.csv",sep=""))

summary(df1)
names(df1)
head(df1)

summary(df2)
names(df2)
head(df2)

summary(df3)
names(df3)
head(df3)

#inner_join to test
names(df_test)<- c("card_no")
length(unique(df_test$card_no))
head(df_test)
df_test <- inner_join(df_test, df1, by = c("card_no","card_no"))
df_test <- inner_join(df_test,df2,by=c("cst_id","cst_id"))
#df_test <- inner_join(df_test,df3,by=c("card_no","card_no"))
# check unique card_no
length(unique(df_test$card_no))



#left join to train
names(df_train)<- c("card_no","label")
head(df_train)
length(unique(df_train$card_no))
df_train <- inner_join(df_train, df1, by = c("card_no","card_no"))
df_train <- inner_join(df_train,df2,by=c("cst_id","cst_id"))
#df_train <- inner_join(df_train,df3,by=c("card_no","card_no"))
# check unique card_no
length(unique(df_train$card_no))


# factor for visualized

#df_train$card_no<-factor(df_train$card_no)
#df_train$bill_cyc <- factor(df_train$bill_cyc)
#df_train$cr_lmt_amt<-factor(df_train$cr_lmt_amt)
#df_train$prev_cr_lmt_amt<-factor(df_train$prev_cr_lmt_amt)
#df_train$cst_id<-factor(df_train$cst_id)

#df_test$card_no<-factor(df_test$card_no)
#df_test$bill_cyc <- factor(df_test$bill_cyc)
#df_test$cr_lmt_amt<-factor(df_test$cr_lmt_amt)
#df_test$prev_cr_lmt_amt<-factor(df_test$prev_cr_lmt_amt)
#df_test$cst_id<-factor(df_test$cst_id)

