library( rpart )
library( rpart.plot )
library( ROCR )



PATH 		= "C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed"
FILE_NAME 	= "HMEQ_Scrubbed.csv"


setwd( PATH )
df = read.csv( FILE_NAME )


str(df)


summary(df)
head(df)


df_amt = df
df_amt$TARGET_BAD_FLAG = NULL


#Regression Decision Tree
SEED = 1
set.seed( SEED )


summary(df_amt)



#FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.8,0.2))
#FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.7,0.3))
#FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.6,0.4))
FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.9,0.1))


df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]



mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )



#Decision-Tree
#tr_set = rpart.control( maxdepth = 5 )
#tr_set = rpart.control( maxdepth = 3 )
#tr_set = rpart.control( maxdepth = 2 )
tr_set = rpart.control( maxdepth = 4 )



#Decision-Tree-1
t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )



#Decision-Tree-2
t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )


#Variable-Importance

t1a$variable.importance
t1p$variable.importance




# TRAIN-DATA-Prediction

p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )


#Train-RMSE

print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )



# TEST-DATA-Prediction

p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )


#Test-RMSE

print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )




# ALL DATA

t1a = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )


t1p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )


#Variable-Importance

t1a$variable.importance
t1p$variable.importance


#Prediction

p1a = predict( t1a, df_amt )
RMSE1a = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_amt )
RMSE1p = sqrt( mean( ( df_amt$TARGET_LOSS_AMT - p1p )^2 ) )


#Full-Data-RMSE

print( paste("ALL DATA RMSE ANOVA =", RMSE1a) )

print( paste("ALL DATA RMSE POISSON =", RMSE1p) )


