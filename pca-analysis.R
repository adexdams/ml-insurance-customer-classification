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

df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL

head(df_pca)


pca2 = prcomp(df_pca[,c(1,2,4,6,10,12,16,18)] ,center=TRUE, scale=TRUE)
summary(pca2)

plot(pca2, type = "l")
df_new = predict( pca2, df_pca )

print(pca2$rotation)



#Plotting scatter-plots with the pca created

df_pca$PC1 = df_new[,"PC1"]
df_pca$PC2 = df_new[,"PC2"]

head( df_pca )



# Define colors for "defaults" and "non-defaults"
colors <- c("black", "red")  # black for 0 (non-defaults), red for 1 (defaults)

# Map colors to the TARGET_BAD_FLAG values
point_colors <- colors[df$TARGET_BAD_FLAG + 1]

# Create the scatter plot
plot(df_pca$PC1, df_pca$PC2, col = point_colors, pch = 16,
     xlab = "Principal Component 1", 
     ylab = "Principal Component 2",
     main = "Scatter Plot of PC1 v vPC2")


# legend to differentiate between defaults and non-defaults
legend("topright",
       legend = c("Non-Defaults (0)", "Defaults (1)"),
       col = c("black", "red"),
       pch = 16)
 



#Selecting a random Sample

df_no_flags = df
df_no_flags$PC1 = df_new[,"PC1"]
df_no_flags$PC2 = df_new[,"PC2"]
df_no_flags$PC3 = df_new[,"PC3"]
df_no_flags$PC4 = df_new[,"PC4"]


df_no_flags$RAND1 = sample(100, size = nrow(df_no_flags), replace = TRUE)
df_no_flags$RAND2 = sample(100, size = nrow(df_no_flags), replace = TRUE)


df_no_flags0 = df_no_flags[ which(df_no_flags$TARGET_BAD_FLAG == 0), ]
df_no_flags1 = df_no_flags[ which(df_no_flags$TARGET_BAD_FLAG == 1), ]

df_no_flags0 = df_no_flags0[ df_no_flags0$RAND1 < 25, ]
df_no_flags1 = df_no_flags1[ df_no_flags1$RAND1 < 75, ]

df_no_flagsx = rbind( df_no_flags0, df_no_flags1 )
df_no_flagsx = df_no_flagsx[ df_no_flagsx$RAND2 < 15, ]

#df_no_flagsx = df_no_flags

#colors <- c("#00AFBB", "#E7B800")
colors <- c("black", "red")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC2, col=colors, pch=16,
      xlab = "PCA 1", 
      ylab = "PCA 2",
      main = "Scatter Plot of PC1 v PC2")

#colors <- c("#00AFBB", "#E7B800")
colors <- c("black", "red")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC3, col=colors, pch=16,
      xlab = "PCA 1", 
      ylab = "PCA 3",
      main = "Scatter Plot of PC1 v PC3")

#colors <- c("#00AFBB", "#E7B800")
colors <- c("black", "red")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC4, col=colors, pch=16,
      xlab = "PCA 1", 
      ylab = "PCA 4",
      main = "Scatter Plot of PC1 v PC4")