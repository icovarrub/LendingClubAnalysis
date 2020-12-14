library(dplyr)
library(caret)
library(lattice)
library(ggplot2)
library(MASS)
library(e1071)
library(psych)
library(corrplot)
library(pROC)
library(ISLR)
library(doParallel)
library(lubridate)

system.time(loanData <- fread("2007-2015_LoanStats.csv"))
str(loanData)

loanData_subset <- select(loanData,
                          acc_now_delinq,
                          acc_open_past_24mths,
                          addr_state,
                          all_util,
                          annual_inc,
                          annual_inc_joint,
                          application_type,
                          avg_cur_bal,
                          bc_open_to_buy,
                          bc_util,
                          chargeoff_within_12_mths,
                          collections_12_mths_ex_med,
                          delinq_2yrs,
                          delinq_amnt,
                          earliest_cr_line,
                          emp_length,
                          funded_amnt,
                          grade,
                          home_ownership,
                          il_util,
                          inq_fi,
                          inq_last_12m,
                          inq_last_6mths,
                          installment,
                          int_rate,
                          issue_d,
                          loan_amnt,
                          loan_status,
                          max_bal_bc,
                          acc_now_delinq,
                          acc_now_delinq,
                          acc_now_delinq,
                          acc_now_delinq,
                          acc_now_delinq,
                          acc_now_delinq,
                          acc_now_delinq,
                          mths_since_rcnt_il,
                          mths_since_recent_bc,
                          mths_since_recent_bc_dlq,
                          mths_since_recent_inq,
                          mths_since_recent_revol_delinq,
                          num_accts_ever_120_pd,
                          num_actv_bc_tl,
                          num_actv_rev_tl,
                          num_bc_sats,
                          num_bc_tl,
                          num_il_tl,
                          num_op_rev_tl,
                          num_rev_accts,
                          num_rev_tl_bal_gt_0,
                          num_sats,
                          num_tl_120dpd_2m,
                          num_tl_30dpd,
                          num_tl_90g_dpd_24m,
                          num_tl_op_past_12m,
                          open_acc,
                          open_acc_6m,
                          open_il_12m,
                          open_il_24m,
                          open_act_il,
                          open_rv_12m,
                          open_rv_24m,
                          out_prncp,
                          pct_tl_nvr_dlq,
                          percent_bc_gt_75,
                          pub_rec,
                          pub_rec_bankruptcies,
                          purpose,
                          pymnt_plan,
                          revol_bal,
                          revol_util,
                          tax_liens,
                          term,
                          tot_coll_amt,
                          tot_cur_bal,
                          tot_hi_cred_lim,
                          total_acc,
                          total_bal_ex_mort,
                          total_bal_il,
                          total_bc_limit,
                          total_cu_tl,
                          total_il_high_credit_limit,
                          total_pymnt,
                          total_rev_hi_lim,
                          verification_status,
                          zip_code,
                          hardship_flag)

aggr_plot <- aggr(loanData_subset, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, label=names(loanData_subset), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

missPropcounts <- data.frame(matrix(ncol = 2, nrow = 101 )) #create data frame to put in prop of missing data info into
missPropcounts$X1 <- names(loanData_x) #populate column with names from loan data frame

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) #specify function to round decimals
missPropcounts$X2 <- specify_decimal(sapply(loanData_x, function(x) (sum(is.na(x))/nrow(loanData_x))), 4) #find proportion of missing data in column to two dec places and load into data frame


#####Impute missing data####
library(mice)
tempData <- mice(loanData_subset, m=1, maxit=1, meth='cart', seed=100)
densityplot(tempData) #view density plot of imputed data compared to observed data

completedData <- complete(tempData, 1) #move imputed data into a complete data set with observed data
saveRDS(completedData, "Imputed-loan-data.RDS") #save imputed data into datafile


###Set working directory
setwd("c:/users/covarrubias/documents/2017 Fall_620/")

#Clean up fully imputed data set
#Y already in YES/NO format
#Subsetting Data and only working on data from 2015 (computation efficiency)
loan_data <- read.csv("loans_sub.csv")
saveRDS(loan_data, "loans_sub.RDS")
saveRDS(loans.x, "loans_subx.RDS")

loans <- readRDS("loans_sub.RDS")  #Already Imputed

str(loans)
names(loans)

#Split out y and predictors
loans.y <- loans$loan_status  #what we want to model
loans.x <- loans[ , c(2:26, 28:77)]  #predictors
str(loans.y)
str(loans.x)

########################PREPROCESSING#####################################################################
#####There are columns with characters that need to be taken out
#####Omit a couple of columns not needed for this analysis
#####There are dates that must be cleaned up as well

#NULL State because it is not needed in order to predict good vs bad loan
#NULL Zip code because the last 3 digits are missing - not needed for this analysis
loans.x$zip_code <- NULL

#Delete # in columns
loans.x$revol_util <- gsub("%","",loans.x$revol_util)
loans.x$revol_util <- as.numeric(loans.x$revol_util)
str(loans.x$revol_util)

loans.x$int_rate <- gsub("%","",loans.x$int_rate)
loans.x$int_rate <- as.numeric(loans.x$int_rate)
str(loans.x$int_rate)

#Earliest credit line is provided, but not in a useful format.
#Issued date is provided, and we can calculate the number of days from issue to earliest credit line.
head(cbind(loans.x$issue_d, loans.x$earliest_cr_line))
loans.x$issue_d <- mdy(loans.x$issue_d)
loans.x$earliest_cr_line <- mdy(loans.x$earliest_cr_line)
loans.x$number_of_days <- loans.x$issue_d - loans.x$earliest_cr_line
head(loans.x$number_of_days)
loans.x$number_of_days <- as.numeric(loans.x$number_of_days)
str(loans.x)

#Now that we have a "number_of_days" column, we can omit issued_d and earliest cr line
#Both were subtracted giving the number of days from issued loan to earliest cr line date
#easier format to work with.
loans.x$issue_d <- NULL
loans.x$earliest_cr_line <- NULL
str(loans.x)

#convert factors to dummy coded predictors
#using caret create a model of the dummy variables, with Full rank so N factor levels-1 new predictors
loans.dmodel <- dummyVars("~ .", data=loans.x, fullRank=TRUE)

#apply model to data with predict function
loans.df <- data.frame(predict(loans.dmodel, loans.x))

#compare data frames original with factors against dummy coded version
#check variables names... could causes problems in modeling
#may need to rename if odd charcters show up
names(loans)
names(loans.df)

saveRDS(loans.df, "loans.df_X.RDS")
saveRDS(loans.y, "loans.Y.RDS")

#test both with logistic regression using 10-Fold CV

ctrl <- trainControl(method="cv", number=10,
                     classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = twoClassSummary, #binary class
                     allowParallel =  FALSE) #bug in caret

#Register core backend, using 8-10 cores on r studio server
cl <- makeCluster(12)
registerDoParallel(cl)

##Test with Logistic Regression before balancing the data
set.seed(50)
loans.glm<- train(y=loans.y, x=loans.df,
                  trControl = ctrl,
                  metric = "ROC", #using AUC to find best performing parameters
                  method = "glm")
loans.glm
getTrainPerf(loans.glm)
summary(loans.glm) #model summary
varImp(loans.glm) #rank important variables

#Confirmed that the data set is good for modeling
#Saving cleaned x and y files
#Time to balance the data

stopCluster(cl)

loans.y <- readRDS("loans.Y.RDS")  #save data frames
loans.df <- readRDS("loans.df_X.RDS")  #save data frames

################Dealing with Class Imbalance####################
#Relevel loan status so "yes" becomes the positive class for a bad loan
loans.y <- relevel(loans.y, ref="YES")

#See unbalanced data
loans.y.table <- table(loans.y)
prop.table(loans.y.table) # .15% of cases are Yes - default and charge-off on a loan

#we must balance the data with both Rose and Smote and compare performances
#first, we have to partition the data for training and testing
#Rose and smote can only be executed on the training data
set.seed(50)
#doing a .70 and .30 split
inTrain<-createDataPartition(y=loans.y, p=.70, list=FALSE)

loans.y.train <- loans.y[inTrain]
loans.x.train <- loans.df[inTrain,]

loans.y.test <- loans.y[-inTrain]
loans.x.test <- loans.df[-inTrain,]

str(loans.y.train)
View(loans.y.train)
#check composition
prop.table(table(loans.y.train))
prop.table(table(loans.y.test))

#recombine y with features (training)
loans.train <- cbind(loans.y.train, loans.x.train)
str(loans.train) #double check they combined correctly

#Balance Smote and Rose
library(DMwR)
library(ROSE)

cl <- makeCluster(14)
registerDoParallel(cl)

set.seed(50)

smote_train <- SMOTE(loans.y.train ~ ., data = loans.train)                     
prop.table(table(smote_train$loans.y.train)) 
smote_train$loans.y.train

set.seed(50)
rose_train <- ROSE(loans.y.train ~ ., data = loans.train)$data
prop.table(table(rose_train$loans.y.train))
rose_train$loans.y.train <- relevel(rose_train$loans.y.train, ref="YES")

saveRDS(smote_train, "smote_train.RDS") #save smote DF
saveRDS(rose_train, "rose_train.RDS") #save rose DF

rose_train <- readRDS("rose_train.RDS")

#Test both Smoted and Rosed data frames on Logistic Regression

ctrl <- trainControl(method="cv", number=10,
                     classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = twoClassSummary, #binary class
                     allowParallel =  FALSE) #bug in caret

#Smote
set.seed(50)
Lsmote.glm<- train(loans.y.train ~ ., data = smote_train,
                   trControl = ctrl, metric = "ROC", method = "glm")

Lsmote.glm
getTrainPerf(Lsmote.glm)
summary(Lsmote.glm) #model summary
varImp(Lsmote.glm) #rank important variables

Lsmote.p <- predict(Lsmote.glm, loans.x.test)
confusionMatrix(Lsmote.p, loans.y.test)

#Rose
set.seed(50)
Lrose.glm<- train(loans.y.train ~ ., data = rose_train,
                  trControl = ctrl, metric = "ROC", method = "glm")
Lrose.glm
getTrainPerf(Lrose.glm)
summary(Lrose.glm) #model summary
varImp(Lrose.glm) #rank important variables

Lrose.p <- predict(Lrose.glm, loans.x.test)
Lrose.p <- relevel(Lrose.p, ref="YES")
confusionMatrix(Lrose.p, loans.y.test)

#compare smote and rose training performances
bValues <- resamples(list(smote=Lsmote.glm, rose=Lrose.glm))

summary(bValues)

#create plot comparing them
bwplot(bValues, scales=list(relation="free"), xlim=list(c(0,1),c(0,1),c(0,1)), metric="ROC")
bwplot(bValues, scales=list(relation="free"), xlim=list(c(0,1),c(0,1),c(0,1)), metric="Sens") #Sensitvity
bwplot(bValues, scales=list(relation="free"), xlim=list(c(0,1),c(0,1),c(0,1)), metric="Spec")

#lets calculate AUC and roc curves via testing data to determine which set to use
#need probability predictions
Lsmote.prob <- predict(Lsmote.glm, loans.x.test, type="prob")
Lrose.prob <- predict(Lrose.glm, loans.x.test, type="prob")
#using Yes probability as positive class
#predicting default and charge-off - bad loan
Lsmote.roc<- roc(loans.y.test, Lsmote.prob$YES)
Lrose.roc <- roc(loans.y.test, Lrose.prob$YES)
#show auc
auc(Lsmote.roc)
auc(Lrose.roc)

#let's create an ROC plot with all combined
plot(Lsmote.roc, col="black")
plot(Lrose.roc, add=T, col="red")
legend(x=.34, y=.3, cex=1, legend=c("Smote","Rose"), col=c("black", "red"), lwd=5)

###Based on the performance on the test data, Smote is clearly overfitted
###We will model with the Rose Training Data

rose_train <- readRDS("rose_train.RDS")

#Naive Bayes
set.seed(50)
Lrose.nb <- train(loans.y.train ~ ., data = rose_train,
                  trControl = ctrl, metric = "ROC", method = "nb", preProc = c("center", "scale"))
Lrose.nb
getTrainPerf(Lrose.nb)

varImp(Lrose.nb)
plot(Lrose.nb)
Lrose.nb.p <- predict(Lrose.nb, loans.x.test)
Lrose.nb.p <- relevel(Lrose.nb.p, ref="YES")
confusionMatrix(Lrose.nb.p, loans.y.test)

#Bagging
library(ipred)
set.seed(50)
Lrose.bag <- train(loans.y.train ~ ., data = rose_train,
                   trControl = ctrl, metric = "ROC", method = "treebag")

Lrose.bag
Lrose.bag.p <- predict(Lrose.bag, loans.x.test)
confusionMatrix(Lrose.bag.p, loans.y.test)

#RandomForest
library(randomForest)
set.seed(50)
Lrose.rf <- train(loans.y.train ~ ., data = rose_train,
                  trControl = ctrl, metric = "ROC", method = c("rf"))
Lrose.rf

Lrose.rf.p <- predict(Lrose.rf, loans.x.test)
confusionMatrix(Lrose.rf.p, loans.y.test)

#Boosting
library(ada)
set.seed(50)
Lrose.ada <- train(loans.y.train ~ ., data = rose_train, trControl = ctrl, metric = "ROC", method = "ada")

Lrose.ada
plot(Lrose.ada)
Lrose.ada.p <- predict(Lrose.ada, loans.x.test)
confusionMatrix(Lrose.ada.p, loans.y.test)

#NeuralNetworks

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#Normalize entire data frame
rose_train$loans.y.train <- as.numeric(rose_train$loans.y.train)
rose_norm <- as.data.frame(lapply(rose_train, normalize))
str(rose_norm)
rose_norm$loans.y.train <- as.numeric(rose_norm$loans.y.train)+1
rose_norm$loans.y.train <- as.factor(rose_norm$loans.y.train)
summary(rose_norm)
boxplot(rose_norm$loans.y.train)
rose_norm$loans.y.train <- as.factor(rose_norm$loans.y.train)
rose_norm$loans.y.train <- ifelse(rose_norm$loans.y.train==1,'YES','NO')
rose_norm$loans.y.train <- as.factor(rose_norm$loans.y.train)
rose_norm$loans.y.train <- relevel(rose_norm$loans.y.train, ref="YES")

# confirm that the range is now between zero and one
summary(rose_norm$loans.y.train)

# compared to the original minimum and maximum
summary(rose_train$loans.y.train)

install.packages("neuralnet")
require(neuralnet)

set.seed(50)
Lrose.nn <- train(loans.y.train ~ ., trControl = ctrl, data = rose_norm, metric = "ROC", method = "nnet")

Lrose.nn
plot(Lrose.nn)
getTrainPerf(Lrose.nn) #good for when the list is too large
Lrose.nn.p <- predict(Lrose.nn, loans.x.test)
confusionMatrix(Lrose.nn.p, loans.y.test)

#svm with radial kernel
library("MLmetrics")
library(kernlab)

#Reload Rose Training Data set because of all the changes we made to it for NN
rose_train <- readRDS("rose_train.RDS")

expand.grid(fL=c(TRUE, FALSE), usekernel=c(TRUE,FALSE),
            adjust=c(TRUE,FALSE))

set.seed(50)

Lrose.svm <- train(loans.y.train~ ., trControl = ctrl,
                   metric = "ROC",
                   preProc = c("scale"), #scale variables
                   data = rose_train, 
                   method = "svmRadial")

#save train model to file to avoid very lengthy train time
saveRDS(Lrose.svm, "LroseModel-svmRadialCaretTrain.rds")

##Ran for days ##Gave up after 4 days  ##SORRY

#GLM with Lasso or Elasticnet Regularization
library(glmnet)
set.seed(50)
Lrose.glmnet <- train(loans.y.train ~ ., trControl = ctrl, data = rose_train, metric="ROC", method="glmnet")
Lrose.glmnet
varImp(Lrose.glmnet)
Lrose.glmnet.p <- predict(Lrose.glmnet, loans.x.test)
confusionMatrix(Lrose.glmnet.p, loans.y.test)

#lets calculate AUC and roc curves via testing data to determine which set to use
#need probability predictions
Lrose.prob <- predict(Lrose.glm, loans.x.test, type="prob")
Lrose.bag.prob <- predict(Lrose.bag, loans.x.test, type="prob")
Lrose.rf.prob <- predict(Lrose.rf, loans.x.test, type="prob")
Lrose.ada.prob <- predict(Lrose.ada, loans.x.test, type="prob")
Lrose.nn.prob <- predict(Lrose.nn, loans.x.test, type="prob")
Lrose.glmnet.prob <- predict(Lrose.glmnet, loans.x.test, type="prob")
#using Yes probability as positive class
#predicting default and charge-off - bad loan
Lrose.roc <- roc(loans.y.test, Lrose.prob$YES)
Lrose.bag.roc <- roc(loans.y.test, Lrose.bag.prob$YES)
Lrose.rf.roc <- roc(loans.y.test, Lrose.rf.prob$YES)
Lrose.ada.roc <- roc(loans.y.test, Lrose.ada.prob$YES)
Lrose.nn.roc <- roc(loans.y.test, Lrose.nn.prob$YES)
Lrose.glmnet.roc <- roc(loans.y.test, Lrose.glmnet.prob$YES)
#show auc
auc(Lrose.roc)
auc(Lrose.bag.roc)
auc(Lrose.rf.roc)
auc(Lrose.ada.roc)
auc(Lrose.nn.roc)
auc(Lrose.glmnet.roc)

#let's create an ROC plot with all combined
plot(Lrose.roc, col="black")
plot(Lrose.bag.roc, add=T, col="red")
plot(Lrose.rf.roc, add=T, col="blue")
plot(Lrose.ada.roc, add=T, col="green")
plot(Lrose.nn.roc, add=T, col="orange")
plot(Lrose.glmnet.roc, add=T, col="blue")

stopCluster(cl)

#compare smote and rose training performances
rValues <- resamples(list(glm=Lrose.glm, bag=Lrose.bag, rf=Lrose.rf, ada=Lrose.ada, nn=Lrose.nn, glmnet=Lrose.glmnet))

summary(rValues)

#create plot comparing them
bwplot(rValues, scales=list(relation="free"), xlim=list(c(0,1),c(0,1),c(0,1)), metric="ROC")
bwplot(rValues, scales=list(relation="free"), xlim=list(c(0,1),c(0,1),c(0,1)), metric="Sens") #Sensitvity
bwplot(rValues, scales=list(relation="free"), xlim=list(c(0,1),c(0,1),c(0,1)), metric="Spec")
############################
###TEXT MINING###
############################

library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(rpart)

#read loan data file with loan descriptions
loan.data <- read.csv("loan_tm.csv")

#save as rdata file to save space and decrease loading times
saveRDS(loan.data, "loan_tm.rdata")

#read rdata file with loan descriptions
loan.sub <- readRDS(file = "loan_tm.rdata")
View(loan.sub)

#convert loan description text to character
loan.sub$desc <- as.character(loan.sub$desc)

str(loan.sub)

head(loan.sub$desc, 20)

#convert to text multibyte encoding to UTF form
loan.sub$desc <- iconv(loan.sub$desc, to="utf-8",sub="")

#remove html entities like <br>; starting with 
loan.sub$desc <- gsub("<br>","", loan.sub$desc)

#remove "Borrower added on"
loan.sub$desc <- gsub("Borrower added on","", loan.sub$desc)

#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
loan.sub$desc  <- gsub('([[:alpha:]])\\1+', '\\1\\1', loan.sub$desc)

#additional cleaning
loan.sub$desc <- gsub("10K","",loan.sub$desc)
loan.sub$desc <- gsub("1M","",loan.sub$desc)
loan.sub$desc <- gsub("A+","",loan.sub$desc)

#additional cleaning removing everything leaving only letters, numbers, and spaces
loan.sub$desc <- gsub("[^a-zA-Z0-9 ]","",loan.sub$desc)

#review loan descriptions now
head(loan.sub$desc,20)

#list of stopwords
stopwords("english")

#create corpus and clean up text
Loan_Corpus <- Corpus(VectorSource(loan.sub$desc))

Loan_Corpus <- tm_map(Loan_Corpus, removeWords, stopwords("english"))
Loan_Corpus <- tm_map(Loan_Corpus, stemDocument)
Loan_Corpus <- tm_map(Loan_Corpus, removePunctuation)
Loan_Corpus <- tm_map(Loan_Corpus, removeNumbers)
Loan_Corpus <- tm_map(Loan_Corpus, stripWhitespace)
Loan_Corpus <- tm_map(Loan_Corpus, removeWords, stopwords("english"))

#create term document matrix (terms as rows, documents as columns)
Ltdm <- TermDocumentMatrix(Loan_Corpus)

#count row (i.e, terms)
#must convert to matrix to work with as tdm
Ltdm$nrow 

#inspect the term document matrix, make sure to subset it is very large 
inspect(Ltdm[1:36, 1:2])

#remove words that are over 99% sparse (i.e., do not appear in 60% of documents)
Ltdm <- removeSparseTerms(Ltdm, 0.99)
Ltdm$nrow #now 36 terms
Ltdm$ncol #887,379 loan desc
inspect(Ltdm[1:36, 1:3])

# define tdm as matrix
m = as.matrix(Ltdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d #frequency of words

# plot wordcloud
set.seed(1)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#credit and loan most frequent lets see what it is associated with
findAssocs(Ltdm, terms = c("credit", "pay"), corlimit = .0) 

#bar chart of frequent words
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#cluster the documents
#find optimal k
wss <- numeric(20) 
for (k in 1:10) wss[k] <- sum(kmeans(Ltdm, centers=k)$withinss)
plot(wss, type="b") #optimal k = 7

loan.kmeans <- kmeans(Ltdm,7)
loan.kmeans$cluster #lets looks at cluster membership

#plotting 
plot(loan.kmeans$cluster)
