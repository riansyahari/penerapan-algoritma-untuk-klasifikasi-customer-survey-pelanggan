bank <- read.csv("~/bank_customer_survey.csv", header = T, sep=",")
#read dataset
data = read.csv("bank_customer_survey.csv", header = TRUE)

#melihat data
View(data)
#mengatasi missing value dengan replace by mean value
rawdata=data
for(i in 1:ncol(rawdata)){
  rawdata[is.na(rawdata[,i]), i] <- mean(rawdata[,i], na.rm = TRUE)
}
View(rawdata)

#mengatasi missing value dengan menghapus NA
rawdata3=rawdata
rawdata3 <- na.omit(rawdata)

View(rawdata3)

#Decision Trees with Package rpart
library(dplyr)

rawdata3 <- rawdata3 %>%
  mutate(across(where(is.character), as.factor))

str(rawdata3)
# partition and create training set and testing set
set.seed(1234)
Kelompok5 <- sample(2, nrow(rawdata3), replace=TRUE, prob=c(0.8, 0.2))

trainData <- rawdata3[Kelompok5==1,]
testData <- rawdata3[Kelompok5==2,]

head(bank,6)
#menggubah data menjadi NA
bank[bank=="unknown"] <- NA
#deskripsi statistik data
summary(bank)
old_Cust_bank<-subset(bank, bank$poutcome != "nonexistent")
str(old_Cust_bank)
new_Cust_bank<-subset(bank, bank$poutcome == "nonexistent")

library(VIM)

aggr_plot <- aggr(old_Cust_bank, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

counts <- table(old_Cust_bank$loan)
barplot(counts,col=c("darkblue","red"),legend = rownames(counts), main = "Pinjaman")

library(mice)

old_imp<-mice(old_Cust_bank)

old_imp_df<-complete(old_imp)

aggr_plot <- aggr(old_imp_df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
old_imp_df$default<-ifelse(old_imp_df$default =="yes",1,0)
old_imp_df$housing<-ifelse(old_imp_df$housing =="yes",1,0)
old_imp_df$loan<-ifelse(old_imp_df$loan =="yes",1,0)
str(old_imp_df)
str(old_imp_df)

data <- old_imp_df
split <- sample(nrow(data), nrow(data)*0.8)
train <- data[split,]
test <- data[-split,]

head(train)
(sum(is.na(train))/(nrow(train)*ncol(train)))*100

library(C50)
model.c50 <- C5.0(formula=y~age+job+marital+education+default+housing+
                    loan+contact+day+month+campaign+previous+poutcome+pdays,data=train, trials=15)
head(test)
library(caret)
confusionMatrix(as.factor(test$y),as.factor(prediksi.c50))
library(ROCR)
pr <- prediction(as.numeric(prediksi.c50), as.numeric(test$y))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc