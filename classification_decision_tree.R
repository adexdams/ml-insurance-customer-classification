
library(rpart)
library(rpart.plot)
library(ROCR)

df = read.csv("C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed/HMEQ_Scrubbed.csv")

str(df)
summary(df)
head(df)

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

head(df_flag)


tr_set = rpart.control(maxdepth = 10)
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )


rpart.plot(t1G)
rpart.plot(t1E)

t1G$variable.importance
t1E$variable.importance


pG = predict( t1G, df )
pG2 = prediction( pG[,2], df$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )


pE = predict( t1E, df )
pE2 = prediction( pE[,2], df$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )



plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY"),col=c("red","green"), bty="y", lty=1 )


aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

aucG
aucE
