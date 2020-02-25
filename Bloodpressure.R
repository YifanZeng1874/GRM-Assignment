#***********************************
#* Blood Pressure Data Analysis    *
#***********************************

Bloodpressure.dat  <- read.table("Bloodpressure.txt",header=T)


fit1 <- lm(recovery_time ~ log_drug + blood_pressure,Bloodpressure.dat)
summary(fit1)
plot(fit1)

fit2 <- lm(log(recovery_time) ~ log_drug + blood_pressure,Bloodpressure.dat)
summary(fit2)
plot(fit2)


set.seed(1)
index <- sample(53,40)
train <- Bloodpressure.dat[index, ]
test <- Bloodpressure.dat[-index,]
fit11 <- lm(recovery_time ~ log_drug + blood_pressure,train)
fit22 <- lm(log(recovery_time) ~ log_drug + blood_pressure,train)

summary(fit11)
summary(fit22)

pred11 <- predict(fit11,test,type= "response")
mse11 <- mean((test$recovery_time - pred11)^2)
sqrt(mse11)

pred22 <- exp(predict(fit22,test,type= "response"))
mse22 <- mean((test$recovery_time - pred22)^2)
sqrt(mse22)
#pred.dt <- data.frame(pre1 = pred11,pre2 = pred22,real = test$recovery_time)


hist(Bloodpressure.dat$recovery_time,xlab = "Recovery time",ylab = "Frequency")
hist(log(Bloodpressure.dat$recovery_time),xlab = "Log(Recovery time)",ylab = "Frequency")

newdata = data.frame(log_drug = 2.00,blood_pressure = 75)
intervalpred22 <- predict(fit2,newdata,interval = "predict")
intervalpred11 <- predict(fit1,newdata,interval = "predict")
