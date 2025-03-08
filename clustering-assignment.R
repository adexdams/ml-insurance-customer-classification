library( rpart )
library( rpart.plot )
library( ROCR )
library( MASS )

library( ggplot2 )
library( flexclust )




SEED = 1
set.seed( SEED )

TARGET = "TARGET_BAD_FLAG"


PATH 		= "C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed"
FILE_NAME 	= "HMEQ_Scrubbed.csv"


setwd( PATH )
df = read.csv( FILE_NAME )



df_pca = df

head( df_pca )


#Removing the target variables
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL


#Removing flag variables
df_pca$M_MORTDUE = NULL
df_pca$M_VALUE = NULL
df_pca$M_YOJ = NULL
df_pca$IMP_DEROG = NULL
df_pca$M_DEROG = NULL
df_pca$M_DELINQ = NULL
df_pca$M_CLAGE = NULL
df_pca$IMP_NINQ = NULL
df_pca$M_NINQ = NULL
df_pca$M_CLNO = NULL
df_pca$M_DEBTINC = NULL
df_pca$FLAG.Job.Mgr = NULL
df_pca$FLAG.Job.Office = NULL
df_pca$FLAG.Job.Other = NULL
df_pca$FLAG.Job.ProfExe = NULL
df_pca$FLAG.Job.Sales = NULL
df_pca$FLAG.Job.Self = NULL
df_pca$FLAG.Reason.DebtCon = NULL
df_pca$FLAG.Reason.HomeImp = NULL


head(df_pca)


#Running a PCA on variables
pca = prcomp(df_pca, center=TRUE, scale=TRUE)
print( pca )
summary(pca)
plot(pca, type = "l")
df_new = data.frame( predict( pca, df_pca ) )


df_kmeans = df_new[1:4]
print( head( df_kmeans ) )
plot( df_kmeans$PC1, df_kmeans$PC2 )



#Finding the optimum number of clusters

# Maximum Clusters To Search
MAX_N = 20

# Set up an array to hold the Sum of Square Errors
WSS = numeric( MAX_N )

for ( N in 1:MAX_N ) 
{
  km = kmeans( df_kmeans, centers=N, nstart=20  )
  WSS[N] = km$tot.withinss
}

df_wss = as.data.frame( WSS )
df_wss$clusters = 1:MAX_N

scree_plot = ggplot( df_wss, aes( x=clusters, y=WSS, group=1 )) +
  geom_point( size=4 ) +
  geom_line() +
  scale_x_continuous( breaks=c(2,4,6,8,10,12,14,16,18,20)) +
  xlab("Number of Clusters")

scree_plot



#Cluster Analysis
BEST_N = 3
km = kmeans( df_kmeans, centers=BEST_N, nstart=20  )

print( km$size )
print( km$centers )


#Converting to flexclust
kf = as.kcca( object=km, data=df_kmeans, save.data=TRUE )
kfi = kcca2df( kf )
agg = aggregate( kfi$value, list( kfi$variable, kfi$group ), FUN=mean )

barplot(kf)



#Scoring the training data
clus = predict( kf, df_kmeans )

plot( df_kmeans$PC1, df_kmeans$PC2, col=clus )
legend( x="topright", legend=c(1:BEST_N), fill=c(1:BEST_N) )

df$CLUSTER = clus
agg = aggregate( df$TARGET_BAD_FLAG, list( df$CLUSTER ), FUN=mean )

print( agg )



#Developing a decision tree
df_tree = df_pca
df_tree$CLUSTER = as.factor(clus)

dt = rpart( CLUSTER ~ . , data=df_tree, maxdepth=4 )

rpart.plot( dt )
