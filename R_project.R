# Importing Dataset
library(MASS)
data_train=read.csv("C:/Users/dixit/Desktop/R project/Training50.csv")
data_test=read.csv("C:/Users/dixit/Desktop/R project/Test50.csv")
# Chcking for the structure of data
str(data_train)
str(data_test)
# Extracts of the first 6 observation
head(data_train)
head(data_test)
# Data preprossing and checking missing value
summary(data_train)
summary(data_test)
sum(is.na(data_train))
sum(is.na(data_test))

# Fitting logistic regression to train set
log_model=glm(formula =Creditability ~., family=binomial,data=data_train)
prediction_1=predict(log_model,type='response',newdata = data_test)
credit_worthy_1=ifelse(prediction_1>.70,1,0)
output_1 <- cbind(data_test, credit_worthy_1 )



summary(log_model)

#-----------------------------------------------------
#First we define a null model and a full model

null_mod=glm(formula=Creditability~1, family = binomial,data=data_train)

full_mod=glm(formula = Creditability ~., family=binomial,data=data_train)

new_model=stepAIC(full_mod,scope = list(lower=null_mod,upper=full_mod),data=data_train,direction = 'backward')
summary(new_model)

#Now we use test data to determine the predictability of our model
# first we built model on train data
model_2=glm(formula = Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Value.Savings.Stocks+Instalment.per.cent+Sex...Marital.Status +Guarantors+Most.valuable.available.asset+Telephone,family = binomial,data = data_train)
prediction_2=predict(model_2,type='response',newdata = data_test)
credit_worthy_2=ifelse(prediction_2>.70,1,0)
output_2 <- cbind(data_test, credit_worthy_2 )
#---------------------------------------
# Calculating AUC:
# Model with all variables:
library(ROCR)
library(Metrics)
pr_1=prediction(credit_worthy_1,data_test$Creditability)
perf_1=performance(pr_1,measure='tpr',x.measure='fpr')
plot(perf_1)
auc(data_test$Creditability,credit_worthy_1)

# Calculating AUC for backward stepwise regression model.
pr_2=prediction(credit_worthy_2,data_test$Creditability)
perf_2=performance(pr_2,measure='tpr',x.measure='fpr')
plot(perf_2)
auc(data_test$Creditability,credit_worthy_2)
#-----------------------------------------------
#Linear discriminant Analysis
# Fit the model
model_lda=lda(formula = Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Value.Savings.Stocks+Instalment.per.cent+Sex...Marital.Status +Guarantors+Most.valuable.available.asset+Telephone,data = data_train)
model_lda
# Make predictions
pred_lda=predict(model_lda,data=data_test)

# Confusion Matrix
cf=table(pred_lda$class,data_test$Creditability)
library(caret)
confusionMatrix(cf)
