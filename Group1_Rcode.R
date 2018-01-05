#READING THE DATA SETS

setwd('/Users/ShikharMittal/Downloads')
#reading the dataset 1
churn= read.csv('churn1.csv')
#reading the dataset 2
churn2 = read.csv('churn2.csv') 

table(churn$gender, churn$Churn)

table(churn$SeniorCitizen,churn$Churn)

#Churn rate for Senior Citizens is high. 
colnames(churn)

table(churn$PhoneService,churn$Churn)

table(churn$Dependents,churn$Churn)

table(churn$PhoneService,churn$Churn)

#WORKING THE DATA SET 1
#Splitting the dataset into train and test by taking 75% of the data as train and 25% as test

smp_size <- floor(0.75 * nrow(churn))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(churn)), size = smp_size)

train <- churn[train_ind, ]
test <- churn[-train_ind, ]

#Logistic Regression
model1 = glm(Churn ~ . - customerID, family='binomial',data=train)
summary(model1)

#Tree
tree_c = tree(Churn ~ . - customerID, data=train)
summary(tree_c)

plot(tree_c)
text(tree_c,pretty=0)
tree_c


#discount proposal to reduce senior citizen churn rate
seniorsdata <- churn1[churn1$SeniorCitizen == '1',]
head(seniorsdata)
library(tree)

#logistic regression 
model3 = glm(Churn ~ . - customerID, family='binomial',data=seniorsdata)
summary(model3)

#decision tree 
tree_d = tree(Churn ~ . - customerID, data=seniorsdata)
summary(tree_d)

plot(tree_d)
text(tree_d,pretty=0)


#senior poeple are more affected by tenure with values between 3.5 and 55.5 being significance

train_size <- floor(0.75 * nrow(seniorsdata))
## set the seed to make your partition reproducible
set.seed(4650)
train_ind <- sample(seq_len(nrow(seniorsdata)), size = train_size)
train_senior <- seniorsdata[train_ind, ]
test_senior <- seniorsdata[-train_ind, ]
summary(train_senior)

seniors_avg_monthly = mean(seniorsdata$MonthlyCharges)
seniors_avg_monthly

#first try with 20% discount rate
discount_rate=0.2  #20% discount on monthly bill
effectivness = 0.1 #10% of seniors will be affected
#cost for the predicitcon

# cost if we do nothing 
classficationTable_seniors=table(truth=train_senior$Churn)
classficationTable_seniors 
costs_1=  c(0,-seniors_avg_monthly)
costs_do_nothing= sum(classficationTable_seniors * costs_1)
costs_do_nothing  # the cost of doing no promo with seniors is -21269.62 , we should check if the promotion will have a smaller cost than this
# cost when providing promo to seniors who's tenure is between 3.5 and 55.5
TN=0
FN=-seniors_avg_monthly -250  #250 being the cost of acquiring a new customer plus ads costs  point of sales...
FP= -(discount_rate*seniors_avg_monthly )
TP= (seniors_avg_monthly -discount_rate*seniors_avg_monthly ) * effectivness

cost_with_promo =  c(c(TN,FN),c(FP,TP))
tenure_Pred_train =  train_senior$tenure<55.5

classficationTable = table(truth=train_senior$Churn,
                           predict=  tenure_Pred_train)
classficationTable 

cost_results = sum(classficationTable * cost_with_promo )
cost_results

# whats is the best promotion to provide 
cost=vector("numeric",8)
#range the discount form 10% to 80%
discount_range= seq(0.1, 0.8, by = 0.1)
ind=1
for (p in  discount_range)
{
  discount_rate=p
  effectivness= p*0.15   # assume that discount rate and effectiveness are correlated
  
  FN=-seniors_avg_monthly -200  #200 being the cost of acquiring a new customer plus ads costs  point of sales...
  FP= -discount_rate*seniors_avg_monthly
  TP= (seniors_avg_monthly -discount_rate*seniors_avg_monthly ) * effectivness
  cost_with_promo =  c(c(TN,FN),c(FP,TP))
  
  classficationTable = table(truth=train_senior$Churn,
                             predict=  tenure_Pred_train)
  cost[ind]=sum(classficationTable * cost_with_promo )
  ind=ind+1
}
cost
plot(discount_range, cost, col ="blue", pch = 15, xlab = "Discount", ylab= "Cost")

#month to month customers- discount plan 

newdata_mtm = churn1[churn1$Contract=="Month-to-month",]
Churn= ifelse(newdata_mtm$Churn=='Yes',1,0)
train_size_mtm <- floor(0.75 * nrow(newdata_mtm))
set.seed(4650)
train_ind_mtm <- sample(seq_len(nrow(newdata_mtm)), size = train_size_mtm)
train_mtm <- newdata_mtm[train_ind_mtm, ]
test_mtm <- newdata_mtm[-train_ind_mtm, ]

#logistic model 
model_mtm = glm(Churn ~SeniorCitizen+tenure+MultipleLines+PaperlessBilling+PaymentMethod+TotalCharges, family='binomial',data=train_mtm)
summary(model_mtm)
proba=predict(model_mtm,newdata = test_mtm ,type='response')
results=ifelse(proba > 0.5,1,0)
table(proba)
newdata_mtm[, c(1:21)] <- sapply(newdata_mtm[, c(1:21)], as.numeric)
typeof(newdata_mtm$Contract)
table(factor(proba, levels = min(test_mtm):max(test_mtm)), factor(test_mtm, levels = min(test_mtm):max(test_mtm)))

#prediction table
library(caret)
table(test_mtm$Churn,proba>0.5)
#FALSE TRUE
#No    425  130
#Yes   193  221

#ROC predictions and AUC values are calculated
library(ROCR)
ROCpred = prediction(proba, test_mtm$Churn)
ROCperformance = performance(ROCpred, "tpr", "fpr")
plot(ROCperformance, colorsize = T, printcutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))
AUC = as.numeric(performance(ROCpred, "auc")@y.values)
AUC

#creating a classification tree for this mtm data 
library(tree)
tree_mtm = tree(Churn ~SeniorCitizen+tenure+MultipleLines+PaperlessBilling+PaymentMethod+TotalCharges,data=train_mtm)
summary(tree_mtm)
plot(tree_mtm)
text(tree_mtm,pretty=0)
tree_mtm
mtm_avg_monthly = mean(seniorsdata$MonthlyCharges)
mtm_avg_monthly

discount_rate=0.2  #20% discount on monthly bill
effectivness = 0.15 #15% of seniors will be affected
#cost for the prediciton
#cost if we do nothing 
classficationTable_mtm=table(truth=train_mtm$Churn)
classficationTable_mtm
costs_do_nothing =  c(0,-mtm_avg_monthly)
costs = 67
costs_do_nothing= sum(classficationTable_mtm * costs) 
costs_do_nothing  # the cost of doing no promo with seniors is -194702 
TN=0
FN=-mtm_avg_monthly -200  #200 being the cost of acquiring a new customer plus ads costs  point of sales...
FP= -discount_rate*mtm_avg_monthly  #he would have stayed anyway  and would have payed the true so we lose the difference between the actual price and the discounted price, but we increase loyalty
TP= (mtm_avg_monthly -discount_rate*mtm_avg_monthly ) * effectivness
cost_with_promo =  c(c(TN,FN),c(FP,TP))
tenure_Pred_train = train_mtm$tenure>2.5
classficationTable = table(truth=train_mtm$Churn,
                           predict=  tenure_Pred_train)
cost_results = sum(classficationTable * cost_with_promo )
classficationTable 
#     predict
#truth FALSE TRUE
#No    252 1413
#Yes   372  869
cost_results #[1] -118326.7 #definitely lower than before
# what is the best promotion to provide 
cost=vector("numeric",8)
cost
discount_range= seq(from = 0.1,to = 0.8, by = 0.1)
ind=1
p=vector("numeric", 8)
for (p in  discount_range)
{
  discount_rate = p
  effectivness= p*0.15    # assume that discount rate and effectiveness are correlated
  
  FN=-mtm_avg_monthly -200  #200 being the cost of acquiring a new customer
  FP= -discount_rate*mtm_avg_monthly 
  TP= (mtm_avg_monthly -discount_rate*mtm_avg_monthly ) * effectivness
  cost_with_promo =  c(c(TN,FN),c(FP,TP))
  
  classficationTable = table(truth=train_mtm$Churn,
                             predict=  tenure_Pred_train)
  cost[ind]=sum(classficationTable * cost_with_promo )
  p[ind] = discount_rate
  ind=ind+1
}
cost

#cost
#[1] -114435.4 -124985.7 -135744.1 -146710.5 -157885.1 -169267.8 -180858.5 -192657.4

plot(discount_range, cost, col ="blue", pch = 15, xlab = "Discount", ylab= "Cost")

#WORKING WITH THE SECOND DATA SET

churn2 = read.csv("churn2.csv")
attach(churn2)

#logistic regression
model4 = glm(Churn ~ . - Area.Code - Phone, family='binomial', data=churn2)

#decision tree
library(tree)
tree123=tree(Churn~Day.Mins+Int.l.Plan+CustServ.Calls+Intl.Charge+Intl.Mins+VMail.Message+Intl.Calls+Eve.Mins,data=churn2)
summary(tree123)
tree123
plot(tree123)
text(tree123,pretty=0)

#analyzing that customer churn rate depends on customer service calls 
sum(with(churn2, Churn == 1))
churn_yes = subset(churn2, Churn == 1)
churn_no = subset(churn2, Churn == 0)

sum_yes=sum(churn_yes$CustServ.Calls)
sum_no=sum(churn_no$CustServ.Calls)

churn_yes$percentage_churn=(churn_yes$CustServ.Calls/sum_yes)*100
churn_no$percentage_churn=(churn_no$CustServ.Calls/sum_no)*100

print(churn_yes$percentage_churn)
print(churn_no$percentage_churn)

M1 = mean(churn_yes$percentage_churn)

mean(churn_no$percentage_churn)
#getting the summary of people that have churned and those that have not 
summary(churn_yes$percentage_churn)
summary(churn_no$percentage_churn)

#creating a boxplot to compare both the results simultaneously
boxplot(churn_yes$percentage_churn, churn_no$percentage_churn, data = churn_yes, main = 'Boxplot of the percentage churn obtained',ylab = 'churn percentage',xlab = 'data from churn=1', ylim = c(0,0.9))

#Analyzing the models for their ROC and AUC values
smp_size <- floor(0.75 * nrow(churn2))

## set the seed to make the partition reproductible
set.seed(1234)
train_ind <- sample(seq_len(nrow(churn2)), size = smp_size)

train <- churn2[train_ind, ]
test <- churn2[-train_ind, ]
model4 = glm(Churn ~ CustServ.Calls+Int.l.Plan+Intl.Calls+VMail.Message, family='binomial', data=train)

predicted_results = predict(model4, newdata = test, type = "response" )
results=ifelse(predicted_results > 0.5,1,0)

missclassification_error=mean(results != test$Churn)
print(paste('Accuracy',1-missclassification_error))

table(results,test$Churn)

library(caret)
confusionMatrix(results,test$Churn)

library(ROCR)
ROCpred = prediction(predicted_results, test$Churn)
ROCperformance = performance(ROCpred, "tpr", "fpr")
plot(ROCperformance, colorsize = T, printcutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))
AUC = as.numeric(performance(ROCpred, "auc")@y.values)
AUC

#similar analysis with decision tree for ROC and AUC values
library(tree)
tree_=tree(Churn~VMail.Message+Int.l.Plan+Intl.Calls+Intl.Charge+Intl.Mins+Day.Mins+Day.Charge+Day.Calls+Eve.Calls+Eve.Mins+Eve.Charge, data=train)


summary(tree_)
plot(tree_)
text(tree_,pretty=0)

predicted_results = predict(tree_, newdata = test)
results=ifelse(predicted_results > 0.5,1,0)
missclassification_error=mean(results != test$Churn)
table(results,test$Churn)
a = confusionMatrix(results,test$Churn)

#pruning the decision tree to accomodate only the most significant variables
validated_tree = cv.tree(object = tree_, FUN = prune.tree)
validated_tree 
plot(validated_tree)
plot(x=validated_tree$size, y=validated_tree$dev, type="b")
new_tree <- prune.tree(tree_, best = 4)
plot(new_tree)
text(new_tree, pretty=0)

#checking for the improvement in AUC values after pruning the tree
predicted_results = predict(new_tree, newdata = test)
results=ifelse(predicted_results > 0.5,1,0)
missclassification_error=mean(results != test$Churn)
table(results,test$Churn)
b = confusionMatrix(results,test$Churn)

library(ROCR)
ROCpred = prediction(predicted_results, test$Churn)
ROCperformance = performance(ROCpred, "tpr", "fpr")
plot(ROCperformance, colorsize = T, printcutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))
AUC = as.numeric(performance(ROCpred, "auc")@y.values)
AUC

#PREDICTING THE BEST MODEL BY COST ANALYSIS
set.seed(1234)
smp_size <- floor(0.75 * nrow(churn2))
train_ind <- sample(seq_len(nrow(churn2)), size = smp_size)

train <- churn2[train_ind, ]
test <- churn2[-train_ind, ]

#Tree model
train$Phone = NULL
library(tree)
model1 = tree(Churn ~ . -State,data=train)
validated_tree = cv.tree(object = model1, FUN = prune.tree)
validated_tree 
plot(x=validated_tree$size, y=validated_tree$dev, type="b")
new_tree <- prune.tree(model1, best = 5)
plot(new_tree)
text(new_tree, pretty=0)


preds = predict(new_tree,newdata=test)

cost=list()

j=1
for(i in seq(0,1,0.01)){
  
  
  cost[j]=100*table(test$Churn,preds>i)[2]+500*table(test$Churn,preds>i)[3]+100*table(test$Churn,preds>i)[4]
  
  j=j+1
  
}

print(cost[which.min(cost)])
plot(seq(0,1,0.01),cost)
#Minimum cost = $12,400

#Logistic model

train$Phone = NULL

model2 = glm(Churn ~ . - State - Area.Code,data=train,family='binomial')

test$Phone = NULL

preds_m2 = predict(model2, newdata=test)

cost_m2=list()

j=1
for(i in seq(0,1,0.01)){
  
  
  cost_m2[j]=100*table(test$Churn,preds_m2>i)[2]+500*table(test$Churn,preds_m2>i)[3]+100*table(test$Churn,preds_m2>i)[4]
  
  j=j+1
  
}

print(cost_m2[which.min(cost_m2)])
print(which.min(cost_m2))
plot(seq(0,1,0.01),cost_m2)
#Minimum Cost: $15,400
#Best Threshold: 0.91