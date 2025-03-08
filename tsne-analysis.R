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

dfu = df
dfu$TARGET_BAD_FLAG = NULL
dfu$TARGET_LOSS_AMT = NULL

dfu = unique(dfu)

head(dfu)


#Perplexity Model 30
theTSNE = Rtsne( dfu[,c(1,2,4,6,10,12,16,18)], 
                 dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)


dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]


colors <- c("black", "red")  # black for 0 (non-defaults), red for 1 (defaults)
colors <- colors[df$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16,
      xlab = "TS-1", 
      ylab = "TS-2",
      main = "Scatter Plot of TS-1 v TS-2")

# legend to differentiate between defaults and non-defaults
legend("topright",
       legend = c("Non-Defaults (0)", "Defaults (1)"),
       col = c("black", "red"),
       pch = 16)




#Perplexity Model 100
theTSNE = Rtsne( dfu[,c(1,2,4,6,10,12,16,18)], 
                 dims = 2, perplexity=100, verbose=TRUE, max_iter = 500)


dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]


colors <- c("black", "red")  # black for 0 (non-defaults), red for 1 (defaults)
colors <- colors[df$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16,
      xlab = "TS-1", 
      ylab = "TS-2",
      main = "Scatter Plot of TS-1 v TS-2 @ Perplexity 100")

# legend to differentiate between defaults and non-defaults
legend("topright",
       legend = c("Non-Defaults (0)", "Defaults (1)"),
       col = c("black", "red"),
       pch = 16)




#Perplexity Model 20
theTSNE = Rtsne( dfu[,c(1,2,4,6,10,12,16,18)], 
                 dims = 2, perplexity=20, verbose=TRUE, max_iter = 500)


dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]


colors <- c("black", "red")  # black for 0 (non-defaults), red for 1 (defaults)
colors <- colors[df$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16,
      xlab = "TS-1", 
      ylab = "TS-2",
      main = "Scatter Plot of TS-1 v TS-2 @ Perplexity 20")

# legend to differentiate between defaults and non-defaults
legend("topright",
       legend = c("Non-Defaults (0)", "Defaults (1)"),
       col = c("black", "red"),
       pch = 16)



#Train two Random Forest Models to predict each tSNE values

P = paste(colnames(dfu)[c(1,2,4,6,10,12,16,18)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model_rf = randomForest( data=dfu, F1, ntree=500, importance=TRUE )
ts2_model_rf = randomForest( data=dfu, F2, ntree=500, importance=TRUE )



df_tsne = df

df_tsne$TS1M_RF = predict( ts1_model_rf, df_tsne )
df_tsne$TS2M_RF = predict( ts2_model_rf, df_tsne )



colors <- c("black", "red")  # black for 0 (non-defaults), red for 1 (defaults)
colors <- colors[df$TARGET_BAD_FLAG + 1]
plot( df_tsne$TS1M_RF, df_tsne$TS2M_RF, col=colors, pch=16,
      xlab = "TS-1M", 
      ylab = "TS-2M",
      main = "Scatter Plot of Random Forest TS-1M v TS-2M")

# legend to differentiate between defaults and non-defaults
legend("topright",
       legend = c("Non-Defaults (0)", "Defaults (1)"),
       col = c("black", "red"),
       pch = 16)