library( rpart )
library( rpart.plot )
library( ROCR )



PATH 		= "C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed"
FILE_NAME 	= "HMEQ_Scrubbed.csv"


setwd( PATH )
df = read.csv( FILE_NAME )


str(df)
#view(df)

summary(df)
head(df)


#Classification Decision Tree
SEED = 1
set.seed( SEED )


df_class = df
summary(df_class)


#FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.8,0.2))
#FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.7,0.3))
#FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.8,0.2))
FLAG = sample( c( TRUE, FALSE ), nrow(df_class), replace=TRUE, prob=c(0.9,0.1))
df_train = df_class[FLAG, ]
df_test = df_class[! FLAG, ]

dim( df_class )
dim( df_train )
dim( df_test )

df_train$TARGET_LOSS_AMT = NULL
dim(df_train)


df_test$TARGET_LOSS_AMT = NULL
dim(df_test)

#Decision-Tree
#tr_set = rpart.control( maxdepth = 6 )
#tr_set = rpart.control( maxdepth = 10 )
#tr_set = rpart.control( maxdepth = 10 )
tr_set = rpart.control( maxdepth = 3 )

#Training-set
t1G_train = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, 
             method="class", parms=list(split='gini') )

t1E_train = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, 
             method="class", parms=list(split='information') )

#Plot-chart
rpart.plot( t1G_train )

rpart.plot( t1E_train )


#Variable-Importance
t1G_train$variable.importance

t1E_train$variable.importance



#Building-ROC-Curve

pG = predict( t1G_train, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E_train, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )



plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values


print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )


fG = predict( t1G_train, df_train, type="class" )
fE = predict( t1E_train, df_train, type="class" )

table( fG, df_train$TARGET_BAD_FLAG )
table( fE, df_train$TARGET_BAD_FLAG )




#Test-set
pG = predict( t1G_train, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E_train, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TEST GINI","TEST ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

fG = predict( t1G_train, df_test, type="class" )
fE = predict( t1E_train, df_test, type="class" )

table( fG, df_test$TARGET_BAD_FLAG )
table( fE, df_test$TARGET_BAD_FLAG )
