library( rpart )
library( rpart.plot )
library( ROCR )
library( MASS )

library( Rtsne )
library( randomForest )


SEED = 1
set.seed( SEED )

TARGET = "TARGET_BAD_FLAG"


PATH 		= "C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed"
FILE_NAME 	= "HMEQ_Scrubbed.csv"


setwd( PATH )
df = read.csv( FILE_NAME )



df_model = df
df_model$TARGET_LOSS_AMT = NULL

head( df_model )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_model, TARGET_BAD_FLAG ~ ., 
             control=tr_set, method="class", parms=list(split='gini') )

t1E = rpart( data=df_model, TARGET_BAD_FLAG ~ ., 
             control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance



#Logistic Regression Models

theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_model )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC(theLower_LR, direction="forward", 
                   scope=list(lower=theLower_LR, upper=theUpper_LR))

summary( lr_model )



#Plotting ROC Curves

pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_model )
pE2 = prediction( pE[,2], df_model$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )




plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY","REGRESSION"),
       col=c("red","green","blue"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values

print( aucG )
print( aucE )
print( aucR )

