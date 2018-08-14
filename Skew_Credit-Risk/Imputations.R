#open link http://r-statistics.co/Missing-Value-Treatment-With-R.html for more info


library(rpart)
library(DMwR)
library(Hmisc)
library(mice)
library(randomForest)


creditData <- read.csv('CreditData_Names.csv', header = TRUE)#import credit data
anyNA(creditData)# check for null or NA values

nullReg_df <- data.frame(md.pattern(creditData)) #fetching the missing data pattern from creditData and adding it to Dataframe
write.csv(nullReg_df, file = "NullReg_Data.csv")#Create csv of MD Pattern for better readability

hist(creditData$Account.Status) #plot the missing data column to identify if Median or Mean imputation should be used
hist(creditData$Savings.account.bonds)#plot the missing data column to identify if Median or Mean imputation should be used

original_df <- creditData 


original_df$Account.Status <- impute(original_df$Account.Status, median) #imputing with median method
original_df$Savings.account.bonds <- impute(original_df$Savings.account.bonds, median) #imputing with median method

anyNA(original_df$Account.Status) #check for NA
anyNA(original_df$Savings.account.bonds )


#using knn imputation method refer link for more info on knn
knnOutput <- knnImputation(creditData[, !names(creditData) %in% "default"])

actuals <- original_df$Account.Status[is.na(creditData$Account.Status)]
predicteds <-knnOutput$Account.Status[is.na(creditData$Account.Status)]

regr.eval(actuals,predicteds) # compare MAPE
#       mae         mse        rmse        mape 
#64.658524 5743.453815   75.785578    6.465852

#using rPart method
anova_mod <- rpart(Account.Status ~ . - default, data = creditData[!is.na(creditData$Account.Status),], method = "anova", na.action = na.omit)
rpart_Pred <- predict(anova_mod, creditData[is.na(creditData$Account.Status),])

regr.eval(actuals,rpart_Pred)# compare MAPE
#mae         mse        rmse        mape 
#34.341172 5240.723903   72.392844    3.434117 


#Using Mice method
miceMod <- mice(creditData[, !names(creditData) %in% "default"], method = "rf")
miceOutPut <- complete(miceMod)
anyNA(miceOutPut)


actuals <- original_df$Account.Status[is.na(creditData$Account.Status)]
mice_predict <- miceOutPut[is.na(creditData$Account.Status), "Account.Status"]

regr.eval(actuals, mice_predict)# compare MAPE
#mae         mse        rmse        mape 
#39.517766 7330.710660   85.619569    3.951777 