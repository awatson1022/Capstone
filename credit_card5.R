###libraries and read data###
library(dplyr)
library(woeBinning)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(caret)
library(OneR)
library(randomForest)
library(ggplot2)
library(cowplot)
library(xgboost)
library(gbm)
library(olsrr)
library(pROC)

set.seed(42)

my.path <- 'C:\\Users\\Andrew\\OneDrive\\Documents\\NU Capstone\\';
my.file <- paste(my.path,'credit_card_default.RData',sep='');
out.path <- 'C:\\Users\\Andrew\\OneDrive\\Documents\\NU Capstone\\';
credit_card_default <- readRDS(my.file)

str(credit_card_default)
head(credit_card_default)

# Here is the observation count in each data set;
table(credit_card_default$data.group)

#rename PAY_0 to PAY_1 
names(credit_card_default)[names(credit_card_default) == "PAY_0"] <- "PAY_1"

colSums(is.na(credit_card_default))

credit_card_default %>% count(SEX)
credit_card_default %>% count(EDUCATION)
credit_card_default %>% count(MARRIAGE)
credit_card_default %>% count(data.group)
credit_card_default %>% count(PAY_1)
credit_card_default %>% count(PAY_2)
credit_card_default %>% count(PAY_3)
credit_card_default %>% count(PAY_4)
credit_card_default %>% count(PAY_5)
credit_card_default %>% count(PAY_6)
credit_card_default %>% count(DEFAULT)
6636/(23364+6636)

credit_card_default$EDUCATION[credit_card_default$EDUCATION>4] <- 4
credit_card_default$EDUCATION[credit_card_default$EDUCATION==0] <- 4
credit_card_default$MARRIAGE[credit_card_default$MARRIAGE==0] <- 3

credit_card_default %>% count(EDUCATION)
credit_card_default %>% count(MARRIAGE)

###Feature Engineering###
engineer_data <- cbind(credit_card_default)
str(engineer_data)

#DEFAULT
engineer_data$DEFAULT <- ifelse(test = engineer_data$DEFAULT == 1, yes = "Yes", no = "No")
engineer_data$DEFAULT <- as.factor(engineer_data$DEFAULT)

#SEX
engineer_data$SEX <- ifelse(test = engineer_data$SEX == 1, yes = "Male", no = "Female")
engineer_data$SEX <- as.factor(engineer_data$SEX)

#EDUCATION
engineer_data$EDUCATION <- as.factor(engineer_data$EDUCATION)

#MARRIAGE
engineer_data$MARRIAGE <- as.factor(engineer_data$MARRIAGE)

#Avg_Bill_Amt
engineer_data$Avg_Bill_Amt <- rowMeans(engineer_data[ , c('BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6')], na.rm=TRUE)
summary(engineer_data$Avg_Bill_Amt)

#Avg_Pmt_Amt
engineer_data$Avg_Pmt_Amt <- rowMeans(engineer_data[ , c('PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6')], na.rm=TRUE)
summary(engineer_data$Avg_Pmt_Amt)

#Pmt_Ratio
engineer_data$Pmt_Ratio1 <- engineer_data$PAY_AMT1 / engineer_data$BILL_AMT2
engineer_data$Pmt_Ratio2 <- engineer_data$PAY_AMT2 / engineer_data$BILL_AMT3
engineer_data$Pmt_Ratio3 <- engineer_data$PAY_AMT3 / engineer_data$BILL_AMT4
engineer_data$Pmt_Ratio4 <- engineer_data$PAY_AMT4 / engineer_data$BILL_AMT5
engineer_data$Pmt_Ratio5 <- engineer_data$PAY_AMT5 / engineer_data$BILL_AMT6

summary(engineer_data$Pmt_Ratio1)
summary(engineer_data$Pmt_Ratio2)
summary(engineer_data$Pmt_Ratio3)
summary(engineer_data$Pmt_Ratio4)
summary(engineer_data$Pmt_Ratio5)

engineer_data[,c("Pmt_Ratio1","Pmt_Ratio2","Pmt_Ratio3","Pmt_Ratio4","Pmt_Ratio5")][is.na(engineer_data[,c("Pmt_Ratio1","Pmt_Ratio2","Pmt_Ratio3","Pmt_Ratio4","Pmt_Ratio5")])] <- 1
engineer_data[engineer_data==Inf] <- 1
#Perhaps create a characteristic variable.

summary(engineer_data$Pmt_Ratio1)
summary(engineer_data$Pmt_Ratio2)
summary(engineer_data$Pmt_Ratio3)
summary(engineer_data$Pmt_Ratio4)
summary(engineer_data$Pmt_Ratio5)

#Avg_Pmt_Ratio
engineer_data$Avg_Pmt_Ratio <- rowMeans(engineer_data[,c("Pmt_Ratio1","Pmt_Ratio2","Pmt_Ratio3","Pmt_Ratio4","Pmt_Ratio5")])
summary(engineer_data$Avg_Pmt_Ratio)

#Util
engineer_data$Util1 <- engineer_data$BILL_AMT1 / engineer_data$LIMIT_BAL
engineer_data$Util2 <- engineer_data$BILL_AMT2 / engineer_data$LIMIT_BAL
engineer_data$Util3 <- engineer_data$BILL_AMT3 / engineer_data$LIMIT_BAL
engineer_data$Util4 <- engineer_data$BILL_AMT4 / engineer_data$LIMIT_BAL
engineer_data$Util5 <- engineer_data$BILL_AMT5 / engineer_data$LIMIT_BAL
engineer_data$Util6 <- engineer_data$BILL_AMT6 / engineer_data$LIMIT_BAL

#Avg_Util
engineer_data$Avg_Util <- rowMeans(engineer_data[,c('Util1','Util2','Util3','Util4','Util5','Util6')])
summary(engineer_data$Avg_Util)

#Bal_Growth_6mo
engineer_data$Bal_Growth_6mo <- engineer_data$BILL_AMT1 - engineer_data$BILL_AMT6
summary(engineer_data$Bal_Growth_6mo)

#Util_Growth_6mo
engineer_data$Util_Growth_6mo <- engineer_data$Util1 - engineer_data$Util6
summary(engineer_data$Util_Growth_6mo)

#Max_Bill_Amt
engineer_data$Max_Bill_Amt <- pmax(engineer_data$BILL_AMT1, engineer_data$BILL_AMT2, engineer_data$BILL_AMT3, engineer_data$BILL_AMT4, engineer_data$BILL_AMT5, engineer_data$BILL_AMT6)
summary(engineer_data$Max_Bill_Amt)

#Max_Pmt_Amt
engineer_data$Max_Pmt_Amt <- pmax(engineer_data$PAY_AMT1, engineer_data$PAY_AMT2, engineer_data$PAY_AMT3, engineer_data$PAY_AMT4, engineer_data$PAY_AMT5, engineer_data$PAY_AMT6)
summary(engineer_data$Max_Pmt_Amt)

#Max_DLQ
engineer_data$PAY_1[engineer_data$PAY_1<0] <- 0
engineer_data$PAY_2[engineer_data$PAY_2<0] <- 0
engineer_data$PAY_3[engineer_data$PAY_3<0] <- 0
engineer_data$PAY_4[engineer_data$PAY_4<0] <- 0
engineer_data$PAY_5[engineer_data$PAY_5<0] <- 0
engineer_data$PAY_6[engineer_data$PAY_6<0] <- 0

engineer_data$Max_DLQ <- pmax(engineer_data$PAY_1, engineer_data$PAY_2, engineer_data$PAY_3, engineer_data$PAY_4, engineer_data$PAY_5, engineer_data$PAY_6)
summary(engineer_data$Max_DLQ)

#Max_Util
engineer_data$Max_Util <- pmax(engineer_data$Util1, engineer_data$Util2, engineer_data$Util3, engineer_data$Util4, engineer_data$Util5, engineer_data$Util6)
summary(engineer_data$Max_Util)

#Max_Pmt_Ratio
engineer_data$Max_Pmt_Ratio <- pmax(engineer_data$Pmt_Ratio1, engineer_data$Pmt_Ratio2, engineer_data$Pmt_Ratio3, engineer_data$Pmt_Ratio4, engineer_data$Pmt_Ratio5)
summary(engineer_data$Max_Pmt_Ratio)

#Min_Bill_Amt
engineer_data$Min_Bill_Amt <- pmin(engineer_data$BILL_AMT1, engineer_data$BILL_AMT2, engineer_data$BILL_AMT3, engineer_data$BILL_AMT4, engineer_data$BILL_AMT5, engineer_data$BILL_AMT6)
summary(engineer_data$Min_Bill_Amt)

#Min_Pay_amt
engineer_data$Min_Pmt_Amt <- pmin(engineer_data$PAY_AMT1, engineer_data$PAY_AMT2, engineer_data$PAY_AMT3, engineer_data$PAY_AMT4, engineer_data$PAY_AMT5, engineer_data$PAY_AMT6)
summary(engineer_data$Min_Pmt_Amt)

#Min_DLQ
engineer_data$Min_DLQ <- pmin(engineer_data$PAY_1, engineer_data$PAY_2, engineer_data$PAY_3, engineer_data$PAY_4, engineer_data$PAY_5, engineer_data$PAY_6)
summary(engineer_data$Min_DLQ)

#Min_Util
engineer_data$Min_Util <- pmin(engineer_data$Util1, engineer_data$Util2, engineer_data$Util3, engineer_data$Util4, engineer_data$Util5, engineer_data$Util6)
summary(engineer_data$Min_Util)

#Min_Pmt_Ratio
engineer_data$Min_Pmt_Ratio <- pmin(engineer_data$Pmt_Ratio1, engineer_data$Pmt_Ratio2, engineer_data$Pmt_Ratio3, engineer_data$Pmt_Ratio4, engineer_data$Pmt_Ratio5)
summary(engineer_data$Min_Pmt_Ratio)

###WOE Binning###
#AGE
binning <- woe.binning(engineer_data, 'DEFAULT', 'AGE')
woe.binning.plot(binning)
engineer_data <- woe.binning.deploy(engineer_data, binning)
str(engineer_data)

#Avg_Bill_Amt
binning <- woe.binning(engineer_data, 'DEFAULT', 'Avg_Bill_Amt')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Avg_Pmt_Amt
binning <- woe.binning(engineer_data, 'DEFAULT', 'Avg_Pmt_Amt')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Avg_Pmt_Ratio
binning <- woe.binning(engineer_data, 'DEFAULT', 'Avg_Pmt_Ratio')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Avg_Util
binning <- woe.binning(engineer_data, 'DEFAULT', 'Avg_Util')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Bal_Growth_6mo 
binning <- woe.binning(engineer_data, 'DEFAULT', 'Bal_Growth_6mo')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Util_Growth_6mo
binning <- woe.binning(engineer_data, 'DEFAULT', 'Util_Growth_6mo')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Max_Bill_Amt
binning <- woe.binning(engineer_data, 'DEFAULT', 'Max_Bill_Amt')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Max_Pmt_Amt
binning <- woe.binning(engineer_data, 'DEFAULT', 'Max_Pmt_Amt')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Max_DLQ
binning <- woe.binning(engineer_data, 'DEFAULT', 'Max_DLQ')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Max_Util
binning <- woe.binning(engineer_data, 'DEFAULT', 'Max_Util')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Max_Pmt_Ratio
binning <- woe.binning(engineer_data, 'DEFAULT', 'Max_Pmt_Ratio')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Min_Bill_Amt     
binning <- woe.binning(engineer_data, 'DEFAULT', 'Min_Bill_Amt')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Min_Pmt_Amt    
binning <- woe.binning(engineer_data, 'DEFAULT', 'Min_Pmt_Amt')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Min_DLQ        
binning <- woe.binning(engineer_data, 'DEFAULT', 'Min_DLQ')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Min_Util       
binning <- woe.binning(engineer_data, 'DEFAULT', 'Min_Util')
engineer_data <- woe.binning.deploy(engineer_data, binning)

#Min_Pmt_Ratio         
binning <- woe.binning(engineer_data, 'DEFAULT', 'Min_Pmt_Ratio')
engineer_data <- woe.binning.deploy(engineer_data, binning)

str(engineer_data)

#Create Datasets
cont_data <- subset(engineer_data, select = c(SEX, EDUCATION, MARRIAGE, AGE, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
bin_data <- subset(engineer_data, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt.binned, Avg_Pmt_Amt.binned, Avg_Pmt_Ratio.binned, Avg_Util, Bal_Growth_6mo.binned, Util_Growth_6mo.binned, Max_Bill_Amt.binned, Max_Pmt_Amt.binned, Max_DLQ.binned, Max_Util.binned, Max_Pmt_Ratio.binned, Min_Bill_Amt.binned, Min_Pmt_Amt.binned, Min_DLQ.binned, Min_Util.binned, Min_Pmt_Ratio.binned))
age_data <- subset(engineer_data, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))

###EDA###
#Bar Plots
ggplot(engineer_data, aes(x = SEX, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
plot(engineer_data$SEX)

ggplot(engineer_data, aes(x = EDUCATION, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
plot(engineer_data$EDUCATION)

ggplot(engineer_data, aes(x = MARRIAGE, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
plot(engineer_data$MARRIAGE)

ggplot(engineer_data, aes(x = AGE.binned, fill = DEFAULT)) +
  geom_bar(position='fill')  +
  ylab("Default Rate")
plot(engineer_data$AGE.binned)

ggplot(engineer_data, aes(x = Avg_Bill_Amt.binned, fill = DEFAULT)) +
  geom_bar(position='fill')  +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Avg_Bill_Amt.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Avg_Pmt_Amt.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Avg_Pmt_Amt.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Avg_Pmt_Ratio.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Avg_Pmt_Ratio.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Avg_Util.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Avg_Util.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Bal_Growth_6mo.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Bal_Growth_6mo.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Util_Growth_6mo.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Util_Growth_6mo.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Max_Bill_Amt.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Max_Bill_Amt.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Max_Pmt_Amt.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Max_Pmt_Amt.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Max_DLQ.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Max_DLQ.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Max_Util.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Max_Util.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Max_Pmt_Ratio.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Max_Pmt_Ratio.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Min_Bill_Amt.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Min_Bill_Amt.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Min_Pmt_Amt.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Min_Pmt_Amt.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Min_DLQ.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Min_DLQ.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Min_Util.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Min_Util.binned)) +
  geom_bar()

ggplot(engineer_data, aes(x = Min_Pmt_Ratio.binned, fill = DEFAULT)) +
  geom_bar(position='fill') +
  ylab("Default Rate")
engineer_data %>% 
  ggplot(mapping = aes(x = Min_Pmt_Ratio.binned)) +
  geom_bar()

###Split Data###
#train_data
cont_train <- subset(engineer_data, engineer_data$train ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
bin_train <- subset(engineer_data, engineer_data$train ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt.binned, Avg_Pmt_Amt.binned, Avg_Pmt_Ratio.binned, Avg_Util, Bal_Growth_6mo.binned, Util_Growth_6mo.binned, Max_Bill_Amt.binned, Max_Pmt_Amt.binned, Max_DLQ.binned, Max_Util.binned, Max_Pmt_Ratio.binned, Min_Bill_Amt.binned, Min_Pmt_Amt.binned, Min_DLQ.binned, Min_Util.binned, Min_Pmt_Ratio.binned))
age_train <- subset(engineer_data, engineer_data$train ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
str(cont_train)
str(bin_train)
str(age_train)

#test_data
cont_test <- subset(engineer_data, engineer_data$test ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
bin_test <- subset(engineer_data, engineer_data$test ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt.binned, Avg_Pmt_Amt.binned, Avg_Pmt_Ratio.binned, Avg_Util, Bal_Growth_6mo.binned, Util_Growth_6mo.binned, Max_Bill_Amt.binned, Max_Pmt_Amt.binned, Max_DLQ.binned, Max_Util.binned, Max_Pmt_Ratio.binned, Min_Bill_Amt.binned, Min_Pmt_Amt.binned, Min_DLQ.binned, Min_Util.binned, Min_Pmt_Ratio.binned))
age_test <- subset(engineer_data, engineer_data$test ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
str(cont_test)
str(bin_test)
str(age_test)

#val_data
cont_val <- subset(engineer_data, engineer_data$validate ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
bin_val <- subset(engineer_data, engineer_data$validate ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt.binned, Avg_Pmt_Amt.binned, Avg_Pmt_Ratio.binned, Avg_Util, Bal_Growth_6mo.binned, Util_Growth_6mo.binned, Max_Bill_Amt.binned, Max_Pmt_Amt.binned, Max_DLQ.binned, Max_Util.binned, Max_Pmt_Ratio.binned, Min_Bill_Amt.binned, Min_Pmt_Amt.binned, Min_DLQ.binned, Min_Util.binned, Min_Pmt_Ratio.binned))
age_val <- subset(engineer_data, engineer_data$validate ==1, select = c(SEX, EDUCATION, MARRIAGE, AGE.binned, DEFAULT, Avg_Bill_Amt, Avg_Pmt_Amt, Avg_Pmt_Ratio, Avg_Util, Bal_Growth_6mo, Util_Growth_6mo, Max_Bill_Amt, Max_Pmt_Amt, Max_DLQ, Max_Util, Max_Pmt_Ratio, Min_Bill_Amt, Min_Pmt_Amt, Min_DLQ, Min_Util, Min_Pmt_Ratio))
str(cont_val)
str(bin_val)
str(age_val)

#one_hot
dmy_train <- dummyVars("~.", data = cont_train)
nn_train <- data.frame(predict(dmy_train, newdata = cont_train))
lr_train <- subset(nn_train, select = -c(SEX.Female, EDUCATION.1, MARRIAGE.1, DEFAULT.No))

dmy_test <- dummyVars("~.", data = cont_test)
nn_test <- data.frame(predict(dmy_test, newdata = cont_test))
lr_test <- subset(nn_test, select = -c(SEX.Female, EDUCATION.1, MARRIAGE.1, DEFAULT.No))

dmy_val <- dummyVars("~.", data = cont_val)
nn_val <- data.frame(predict(dmy_val, newdata = cont_val))
lr_val <- subset(nn_val, select = -c(SEX.Female, EDUCATION.1, MARRIAGE.1, DEFAULT.No))

#x & y
cont_train_x <- subset(cont_train, select = -c(DEFAULT))
cont_test_x <- subset(cont_test, select = -c(DEFAULT))
cont_val_x<- subset(cont_val, select = -c(DEFAULT))

bin_train_x <- subset(bin_train, select = -c(DEFAULT))
bin_test_x <- subset(bin_test, select = -c(DEFAULT))
bin_val_x <- subset(bin_val, select = -c(DEFAULT))

age_train_x <- subset(age_train, select = -c(DEFAULT))
age_test_x <- subset(age_test, select = -c(DEFAULT))
age_val_x <- subset(age_val, select = -c(DEFAULT))

nn_train_x  <- subset(nn_train, select = -c(DEFAULT.Yes, DEFAULT.No))
nn_test_x  <- subset(nn_test, select = -c(DEFAULT.Yes, DEFAULT.No))
nn_val_x  <- subset(nn_val, select = -c(DEFAULT.Yes, DEFAULT.No))

cont_train_y <- subset(cont_train, select = c(DEFAULT))
cont_test_y <- subset(cont_test, select = c(DEFAULT))
cont_val_y <- subset(cont_val, select = c(DEFAULT))

bin_train_y <- subset(bin_train, select = c(DEFAULT))
bin_test_y <- subset(bin_test, select = c(DEFAULT))
bin_val_y <- subset(bin_val, select = c(DEFAULT))

age_train_y <- subset(age_train, select = c(DEFAULT))
age_test_y <- subset(age_test, select = c(DEFAULT))
age_val_y <- subset(age_val, select = c(DEFAULT))

nn_train_y <- subset(nn_train, select = c(DEFAULT.Yes, DEFAULT.No))
nn_test_y <- subset(nn_test, select = c(DEFAULT.Yes, DEFAULT.No))
nn_val_y <- subset(nn_val, select = c(DEFAULT.Yes, DEFAULT.No))

#write.csv(nn_train_x, "C:\\Users\\Andrew\\OneDrive\\Documents\\NU Capstone\\nn_train_x", row.names = FALSE)
#write.csv(nn_train_y, "C:\\Users\\Andrew\\OneDrive\\Documents\\NU Capstone\\nn_train_y", row.names = FALSE)
#write.csv(nn_test_x, "C:\\Users\\Andrew\\OneDrive\\Documents\\NU Capstone\\nn_test_x", row.names = FALSE)
write.csv(nn_test_y, "C:\\Users\\Andrew\\OneDrive\\Documents\\NU Capstone\\nn_test_y", row.names = FALSE)



###Logistic Regression###
mylogit <- glm(DEFAULT.Yes ~ ., data = lr_train, family = binomial(link = 'logit'))
summary(mylogit)

backwards = step(mylogit)

lr_model <- glm(DEFAULT.Yes ~ SEX.Male + EDUCATION.4 + MARRIAGE.2 + MARRIAGE.3 + 
  Avg_Bill_Amt + Avg_Pmt_Amt + Bal_Growth_6mo + Max_Bill_Amt + 
  Max_Pmt_Amt + Max_DLQ + Max_Util + Min_Pmt_Amt + Min_DLQ, data = lr_train, family = binomial(link = 'logit'))
lr_model

lr_train_preds <- predict(lr_model, type = "response")
table(lr_train$DEFAULT.Yes, lr_train_preds > 0.5)
(661+11436)/(661+11436+321+2762)
(661)/(661+2762)
321/(11436+321)

lr_test_preds <- predict(lr_model, newdata = lr_test, type = "response")
table(lr_test$DEFAULT.Yes, lr_test_preds > 0.5)
(296+5589)/(296+5589+177+1261)
296/(296+1261)
177/(177+5589)

lr_val_preds <- predict(lr_model, newdata = lr_val, type = "response")

lr_train_preds

decile.pts <- quantile(lr_train_preds,
                       probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));

semi.deciles <- cut(lr_train_preds,breaks=c(0,decile.pts,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
);

semi.deciles



traintable <- table(semi.deciles,lr_train$DEFAULT.Yes)
traintable
traindf <- as.data.frame.matrix(traintable) 
names(traindf)[1] <- "No"
names(traindf)[2] <- "Yes"
traindf <- tibble::rownames_to_column(traindf, "semi.decile")
traindf <- traindf[order(traindf$semi.decile),]
traindf$semi.decile <- as.numeric(traindf$semi.decile)
traindf$cum.No <- cumsum(traindf$No)
traindf$cum.Yes <- cumsum(traindf$Yes)
traindf$cum.No.perc <- (traindf$cum.No/11757)*100
traindf$cum.Yes.perc <- (traindf$cum.Yes/3423)*100
traindf$lift <- traindf$cum.Yes.perc / (5 * traindf$semi.decile)
traindf$KS <- traindf$cum.Yes.perc - traindf$cum.No.perc
traindf

test.decile.pts <- quantile(lr_test_preds,
                       probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));

test.semi.deciles <- cut(lr_test_preds,breaks=c(0,decile.pts,1),
                    labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
);

testtable <- table(test.semi.deciles,lr_test$DEFAULT.Yes)
testtable
testdf <- as.data.frame.matrix(testtable) 
names(testdf)[1] <- "No"
names(testdf)[2] <- "Yes"
testdf <- tibble::rownames_to_column(testdf, "semi.decile")
testdf <- testdf[order(testdf$semi.decile),]
testdf$semi.decile <- as.numeric(testdf$semi.decile)
testdf$cum.No <- cumsum(testdf$No)
testdf$cum.Yes <- cumsum(testdf$Yes)
testdf$cum.No.perc <- (testdf$cum.No/5766)*100
testdf$cum.Yes.perc <- (testdf$cum.Yes/1557)*100
-testdf$lift <- testdf$cum.Yes.perc / (5 * testdf$semi.decile)
testdf$KS <- testdf$cum.Yes.perc - testdf$cum.No.perc
testdf

val.decile.pts <- quantile(lr_val_preds,
                            probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));

val.semi.deciles <- cut(lr_val_preds,breaks=c(0,decile.pts,1),
                         labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
);

valtable <- table(val.semi.deciles,lr_val$DEFAULT.Yes)
valtable
valdf <- as.data.frame.matrix(valtable) 
names(valdf)[1] <- "No"
names(valdf)[2] <- "Yes"
valdf <- tibble::rownames_to_column(valdf, "semi.decile")
valdf <- valdf[order(valdf$semi.decile),]
valdf$semi.decile <- as.numeric(valdf$semi.decile)
valdf$cum.No <- cumsum(valdf$No)
valdf$cum.Yes <- cumsum(valdf$Yes)
valdf$cum.No.perc <- (valdf$cum.No/5841)*100
valdf$cum.Yes.perc <- (valdf$cum.Yes/1656)*100
valdf$lift <- valdf$cum.Yes.perc / (5 * valdf$semi.decile)
valdf$KS <- valdf$cum.Yes.perc - valdf$cum.No.perc
valdf



roc.10 <- roc(response=nn_test$DEFAULT.Yes, predictor=lr_test_preds)
print(roc.10)

# Plot ROC Curve and add AUC to plot
plot(roc.10)
text(0.6,0.6,paste('AUC=',round(auc(roc.10),2),sep=''))


lr_model



#

library(stargazer)
#file.name <- 'trainks.html';
#stargazer(traindf, type=c('html'),out=paste(out.path,file.name,sep=''),
#          title=c('Table XX: Frequency Table of Zone'),
#          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE,
#          summary=FALSE )

#file.name <- 'testks.html';
#stargazer(testdf, type=c('html'),out=paste(out.path,file.name,sep=''),
#          title=c('Table XX: Frequency Table of Zone'),
#          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE,
#          summary=FALSE )

file.name <- 'valks.html';
stargazer(valdf, type=c('html'),out=paste(out.path,file.name,sep=''),
          title=c('Table XX: Frequency Table of Zone'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE,
          summary=FALSE )