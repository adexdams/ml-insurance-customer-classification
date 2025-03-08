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


df_flag = df


FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.8,0.2))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]


dim( df_flag )
dim( df_train )
dim( df_test )

df_train$TARGET_LOSS_AMT = NULL
dim(df_train)

# RF

rf_model = randomForest( data=df_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )




#Loan Amount Prediction - Decision Tree

df_amt = subset( df, TARGET_BAD_FLAG == 1 )
df_amt$TARGET_BAD_FLAG = NULL
dim(df_amt)


mean( df_amt$TARGET_LOSS_AMT )




# REGRESSION-DECISION-TREE

tr_set = rpart.control( maxdepth = 10 )

tr_model = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head( pt )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )


# REGRESSION-RANDOM-FOREST

rf_model_1 = randomForest( data=df_amt, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model_1 )
varImpPlot( rf_model_1 )

pr = predict( rf_model_1, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )



# REGRESSION-GRADIENT BOOSTING

gb_model = gbm( data=df_amt, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )



print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))




#Severity-model
p_sm = pr * pg


#RMSE-severity-model
RMSE_sm = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p_sm )^2 ) )
print( RMSE_sm )



