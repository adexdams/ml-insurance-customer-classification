library("tidyverse")

PATH 		= "C:/Users/Enzo Krypton/Desktop/HMEQ_Scrubbed"
FILE_NAME 	= "HMEQ_Scrubbed.csv"

INFILE = paste( PATH, FILE_NAME, sep="/" )


setwd( PATH )
df = read.csv( FILE_NAME )
head(df)
view(df)


#Task-1
ggplot(df, aes(x = factor(TARGET_BAD_FLAG), fill = factor(TARGET_BAD_FLAG))) +
  geom_bar() +
  labs(title = "Distribution of Loan Defaults", x = "Default Status", y = "Number of observations", fill = "Default Status") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("0" = "GOOD LOAN", "1" = "BAD LOAN")) +
  theme_minimal()

#Task-2
ggplot(df, aes(x = factor(TARGET_BAD_FLAG), y = LOAN, fill = factor(TARGET_BAD_FLAG))) +
  geom_boxplot() +
  labs(title = "Loan Amount by Default Status", x = "Default Status", y = "Loan Amount") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "tomato"),
                    labels = c("0" = "GOOD LOAN", "1" = "BAD LOAN")) +
  theme_minimal()


#Task-3
df %>%
  filter(TARGET_BAD_FLAG == 1) %>%
  ggplot(aes(TARGET_LOSS_AMT))+
  geom_histogram(fill = "lightgreen", colour="black", bins = 30)+
  labs(title = "Loss Amounts for Defaulters", x = "Loss Amount", y = "Count") +
  theme_minimal()



#Task-4
ggplot(df, aes(x = IMP_CLAGE, y = LOAN, color = factor(TARGET_BAD_FLAG))) +
  geom_point(alpha = 0.6) +
  labs(title = "Credit History Length vs Loan Amount by Default Status", x = "Credit History Age", y = "Loan Amount") +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()


#Task-5
df$Job <- with(df, ifelse(FLAG.Job.Mgr == 1, "Mgr",
                              ifelse(FLAG.Job.Office == 1, "Office",
                                     ifelse(FLAG.Job.Other == 1, "Other",
                                            ifelse(FLAG.Job.ProfExe == 1, "ProfExe",
                                                   ifelse(FLAG.Job.Sales == 1, "Sales", "Self"))))))

ggplot(df, aes(x = Job, fill = factor(TARGET_BAD_FLAG))) +
  geom_bar(position = "fill") +
  labs(title = "Default Rate by Employment Type", x = "Employment Type", y = "Proportion of Observations by defaults") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkred"),
                    labels = c("0" = "GOOD LOAN", "1" = "BAD LOAN")) +
  theme_minimal()











