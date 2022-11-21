#health <- read_csv("[path]/healthcare-dataset-stroke-data.csv", col_types = cols(gender = col_factor(levels = c("Male", "Female", "Other")), hypertension = col_factor(levels = c("0", "1")), heart_disease = col_factor(levels = c("0", "1")), ever_married = col_factor(levels = c("No", "Yes")), work_type = col_factor(levels = c("children", "Govt_job", "Never_worked", "Private", "Self-employed")), Residence_type = col_factor(levels = c("Rural", "Urban")), bmi = col_number(), smoking_status = col_factor(levels = c("formerly smoked", "never smoked", "smokes")), stroke = col_factor(levels = c("0", "1"))))
health2 = na.omit(health)
health2 <- health2[,-1] #remove id column

#create model with all predictors
set.seed(500)
fit.health = glm(stroke ~ ., family = "binomial", data=health2)
summary(fit.health)

#create model with significant predictors found from doing stepwise backward method on original model
step(fit.health, direction = "backward")
better.health = glm(formula = stroke ~ age + hypertension + heart_disease + avg_glucose_level, 
    family = "binomial", data = health2)

#create a model w/out heart_disease
best.health = glm(formula = stroke ~ age + hypertension + avg_glucose_level, family = "binomial", data = health2)

#comparing models
#reminder: table w/null dev, residual dev, r^2, AIC for each model

#fit.health info
summary(fit.health)
fit.bic=BIC(fit.health)
fit.stat = cbind(fit.health$null.deviance,fit.health$deviance,(1-(fit.health$deviance/fit.health$null.deviance)),fit.health$aic,fit.bic)
colnames(fit.stat) = c("Null Deviance","Residual Deviance","R^2","AIC","BIC")
#print(fit.stat)
#R^2: 1-(1141.8/1411) =0.1907867

#better.health info
summary(better.health)
better.bic=BIC(better.health)
better.stat = cbind(better.health$null.deviance,better.health$deviance,(1-(better.health$deviance/better.health$null.deviance)),better.health$aic,better.bic)
colnames(better.stat) = c("Null Deviance","Residual Deviance","R^2","AIC","BIC")
#print(better.stat)
#R^2: 1-(1149/1411) =0.1856839

#best.health info
summary(best.health)
best.bic=BIC(best.health)
best.stat = cbind(best.health$null.deviance,best.health$deviance,(1-(best.health$deviance/best.health$null.deviance)),best.health$aic, best.bic)
colnames(fit.stat) = c("Null Deviance","Residual Deviance","R^2","AIC","BIC")
#print(best.stat)
#R^2: 1-(1154.1/1411) = 0.1820695

goodness.fit=rbind(fit.stat,better.stat,best.stat)
rownames(goodness.fit) = c("fit.health","better.health","best.health")
print(goodness.fit)

#some info for explaining why we're picking the predictors from better.health:
#lower residual dev, higher R^2, and lower AIC for better.health shows that including the heart_disease predictor helps to create a better model


#logistic reg. coefficients for equation:

#Intercept:-7.632810
#Age: 0.067773
#hypertension1:0.568379
#heart_disease1:0.453704
#avg_glucose_level: 0.004701


test.error = 0
train.error=0
#train & test:
for (i in 1:10) {
  set.seed(i)
  #Select 80% of the data
  sample = sample.int(n = nrow(health2),size = round(.80*nrow(health2)), replace = FALSE)
  
  train = health2[sample,]
  test = health2[-sample,]
  #create model on training data
  health.train = glm(formula = stroke ~ age + hypertension + heart_disease + avg_glucose_level, family = "binomial", data = train)
  summary(health.train)
  
  #train pred
  prediction.train = predict(health.train, type="response")
  predict.stroke.train = ifelse(prediction.train < 0.5,"0","1")
  (conf.mat.train = table(predict.stroke.train,train$stroke))
  #training error rate:
  rows.train = nrow(conf.mat.train)
  if(rows.train < 2) {
    train.error[i] = (conf.mat.train[1,2]/sum(conf.mat.train))
    #print(error)
  } else {
    train.error[i] = (conf.mat.train[1,2]+conf.mat.train[2,1])/sum(conf.mat.train)
    #print(error)
  }
  #train.error = train.error + error
    
  #test pred
  predict.health = predict(health.train, type = "response", newdata = test)
  predict.stroke = ifelse(predict.health< 0.5,"0","1")
  (conf.test = table(predict.stroke,test$stroke))
  #testing error rate:
  
  rows = nrow(conf.test)
  if(rows < 2) {
    test.error[i] = (conf.test[1,2]/sum(conf.test))
    #print(error)
  } else {
    test.error[i] = (conf.test[1,2]+conf.test[2,1])/sum(conf.test)
    #print(error)
  }
  #test.error = test.error + error
}

#mean of the train error rate
train.error.mean = mean(train.error)
train.error
train.error.mean
#mean of the test error
test.error.mean = mean(test.error)
test.error
test.error.mean
#(test.error/10)


