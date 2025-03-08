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


SEED = 1
set.seed( SEED )


df_class = df
summary(df_class)


FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.8,0.2))

df_train = df_class[FLAG, ]
df_test = df_class[! FLAG, ]

dim( df_class )
dim( df_train )
dim( df_test )

df_train$TARGET_LOSS_AMT = NULL
dim(df_train)




#Bad Loan Prediction - Decision-Tree - Entropy
tr_set = rpart.control( maxdepth = 6 )
#Entropy-model
t1E_train = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, 
                   method="class", parms=list(split='information') )
#Plot-chart
rpart.plot( t1E_train )
#Variable-Importance
t1E_train$variable.importance
#prediction
pE = predict( t1E_train, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )

aucE = performance( pE2, "auc" )@y.values
print(aucE)



#Loan Amount Prediction - Decision Tree
df_amt = subset( df, TARGET_BAD_FLAG == 1 )
df_amt$TARGET_BAD_FLAG = NULL
dim(df_amt)


#Poisson-model
t2_p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
#Plot-chart
rpart.plot( t2_p )
#Prediction
p2_a = predict( t2_p, df_test )
RMSE_p2_a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p2_a )^2 ) )
print(RMSE_p2_a)



#Severity-model
p_sm = pE * p2_a


#RMSE-severity-model
RMSE_sm = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p_sm )^2 ) )
print( RMSE_sm )







#Bad Loan Prediction - Decision-Tree - Gini
tr_set = rpart.control( maxdepth = 6 )
#Entropy-model
t1E_train = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, 
                   method="class", parms=list(split='gini') )
#Plot-chart
rpart.plot( t1E_train )
#Variable-Importance
t1E_train$variable.importance
#prediction
pE = predict( t1E_train, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )

aucE = performance( pE2, "auc" )@y.values
print(aucE)



#Loan Amount Prediction - Decision Tree
df_amt = subset( df, TARGET_BAD_FLAG == 1 )
df_amt$TARGET_BAD_FLAG = NULL
dim(df_amt)


#Poisson-model
t2_p = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
#Plot-chart
rpart.plot( t2_p )
#Prediction
p2_a = predict( t2_p, df_test )
RMSE_p2_a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p2_a )^2 ) )
print(RMSE_p2_a)



#Severity-model
p_sm = pE * p2_a


#RMSE-severity-model
RMSE_sm = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p_sm )^2 ) )
print( RMSE_sm )
