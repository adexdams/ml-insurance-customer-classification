library(rpart)
library(rpart.plot)
library(ROCR)

df = read.csv("C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed/HMEQ_Scrubbed.csv")

str(df)
summary(df)
head(df)


df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

mean(df_amt$TARGET_LOSS_AMT)

tr_set = rpart.control(maxdepth = 10)

t1a = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot(t1a)


t1p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot(t1p)


t1a$variable.importance
t1p$variable.importance


p1a = predict( t1a, df )
p1p = predict( t1p, df )


RMSE1a = sqrt( mean( ( df$TARGET_LOSS_AMT - p1a )^2 ) )
RMSE1p = sqrt( mean( ( df$TARGET_LOSS_AMT - p1p )^2 ) )


RMSE1a
RMSE1p
