install.packages("caret")
install.packages("e1071")
library("caret")
library("e1071")
set.seed(999)
ChurnData <- read.csv("/Users/guan-hunglai/Desktop/Logistic Regression Assignments/telco_churn_data.csv", header = TRUE)
View(ChurnData)
ChurnData <- within(ChurnData, Churn <- relevel(Churn, ref = 'No'))
class(ChurnData$Churn)              

#Drop obs to only 5000
ChurnData <- ChurnData[c(1:5000),]
dim(ChurnData)
mean(ChurnData$SeniorCitizen)

ChurnData_Logistic <- ChurnData[, -5]
ChurnData_Logistic


#Logistic Regression
LogisticReg <- glm(Churn ~ ., data = ChurnData_Logistic, family = "binomial"(link = "logit"))
summary(LogisticReg)

#Probability of Churn
ChurnData_Logistic$churn_prob <- predict.glm(LogisticReg, newdata=ChurnData_Logistic, type="response")
hist(ChurnData_Logistic$churn_prob)

##Porportion of Customers with Churn Rate less than 20%
Lowchurn <- ChurnData_Logistic[which(ChurnData_Logistic$churn_prob <= 0.2),]
Proportion <- dim(Lowchurn)/dim(ChurnData_Logistic) ; Proportion #44.38%

#Q3 113.2/18.95
ChurnData_Logistic[which.max(ChurnData_Logistic$churn_prob), "MonthlyCharges"]
ChurnData_Logistic[which.min(ChurnData_Logistic$churn_prob), "MonthlyCharges"]

min(ChurnData_Logistic)

#Q4 - 104.3572
-LogisticReg$coefficients[6]/LogisticReg$coefficients[9]

#Q5
library('caret')
confusionMatrix(table(1*(ChurnData_Logistic$churn_prob>0.5),1*(ChurnData_Logistic$Churn=='Yes')))

#Q6 Profit Forecast and Optimization
pf<-function(incr, data)
{
  d <- ChurnData_Logistic[ChurnData_Logistic$Churn=="No",]
  d$MonthlyCharges <- d$MonthlyCharges*incr 
  g <- 0.97 # discount factor
  p <- 1-predict.glm(LogisticReg, newdata=d, type="response")
  clv <- d$MonthlyCharges/(1-p*g) 
  return(sum(clv)) 
}

pf(1.0, ChurnData_Logistic)
pf(2.0, ChurnData_Logistic)
set.seed(999)
opt<-optim(1.0, pf, method="L-BFGS-B", control=list(fnscale=-1), data=ChurnData_Logistic)
c(opt$par, opt$value) #par is the optimal increase and value is the optimal profit

#Q7 - Difference in Profit
Diff <- pf(1.0, ChurnData_Logistic) - pf(1.679, ChurnData_Logistic) ; Diff

#Q8 
Diff2 <- pf(0.8, ChurnData_Logistic) - pf(1.2, ChurnData_Logistic) ; Diff2

#Q9
ChurnData_Logistic[4,]
Customer_4_Increase <- ChurnData_Logistic[4,]$MonthlyCharges*1.2
###ChurnData_Logistic[4,]$MonthlyCharges <-50.76
ChurnData_Logistic[4,]$MonthlyCharges <-42.3
ChurnData_Logistic$churn_prob <- predict.glm(LogisticReg, newdata=ChurnData_Logistic, type="response")
1 - ChurnData_Logistic[4,]$churn_prob #0.9248

#Q10
ChurnData_Logistic[4,]$MonthlyCharges <-50.76
ChurnData_Logistic$churn_prob <- predict.glm(LogisticReg, newdata=ChurnData_Logistic, type="response")
predict.glm(LogisticReg, newdata=ChurnData_Logistic, type="response")
