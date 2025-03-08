library( rpart )
library( rpart.plot )
library( ROCR )

library( randomForest )
library( gbm )

SEED = 1
set.seed( SEED )

TARGET = "TARGET_BAD_FLAG"

PATH 		= "C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed"
FILE_NAME 	= "HMEQ_Scrubbed.csv"


setwd( PATH )
df = read.csv( FILE_NAME )


str( df )
summary( df )


df_amt = df
df_amt$TARGET_BAD_FLAG = NULL


FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.7,0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]


mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )



# DECISION-TREE

tr_set = rpart.control( maxdepth = 10 )

tr_model = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head( pt )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )


# RF

rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )



# GRADIENT BOOSTING

gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )



print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))

