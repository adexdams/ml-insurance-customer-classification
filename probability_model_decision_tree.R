library(rpart)
library(rpart.plot)
library(ROCR)

df = read.csv("C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed/HMEQ_Scrubbed.csv")

str(df)
summary(df)
head(df)


df_flag = df
df_flag$TARGET_LOSS_AMT = NULL


tr_set = rpart.control( maxdepth = 10 )


t2_f = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set )
rpart.plot( t2_f )
p2_f = predict( t2_f, df )



df_amt_2 = subset( df, TARGET_BAD_FLAG == 1 )
df_amt_2$TARGET_BAD_FLAG = NULL
head(df_amt_2)



t2_a = rpart( data=df_amt_2, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t2_a )
p2_a = predict( t2_a, df )


p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df$TARGET_LOSS_AMT - p2 )^2 ) )
RMSE1p
RMSE2
