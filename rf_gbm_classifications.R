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
df_flag$TARGET_LOSS_AMT = NULL


FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.6,0.4))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]


dim( df_flag )
dim( df_train )
dim( df_test )


# DECISION-TREE

tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( tr_model )
tr_model$variable.importance

pt = predict( tr_model, df_test, type="prob" )
head( pt )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )



# RF

rf_model = randomForest( data=df_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )




# GRADIENT BOOSTING

gb_model = gbm( data=df_train, TARGET_BAD_FLAG ~ ., n.trees=500, distribution="bernoulli" )
summary.gbm( gb_model, cBars=10 )

pg = predict( gb_model, df_test, type="response" )
head( pg )
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )



plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )



plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )