iris

species_grp = table(iris$Species)
species_grp


#Step 1
str(iris)
summary(iris)
head(iris)



#Step 2
boxplot(iris$Sepal.Length ~ iris$Species, main='Boxplot of Sepal Length vs Species',
        notch=TRUE, col='gold')
boxplot(iris$Sepal.Width ~ iris$Species, main='Boxplot of Sepal Width vs Species',
        notch=TRUE, col='darkgreen')



#Step 3
hist(iris$Sepal.Length, breaks = 5, col='blue')
lines( density(iris$Sepal.Length) )



#Step 4
plot(iris$Petal.Width, iris$Petal.Length, col=iris$Species, pch=16, main="Scatter Plot of numerical variable vs specie")
legend("topleft", legend = c("setosa", "versicolor", "virginica"))



#Step 5
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
sd(iris$Sepal.Length)
a = aggregate( x=iris$Sepal.Length, by=list( iris$Species), FUN=median )
a = a[ order( a$x, decreasing=TRUE), ]
a



mean(iris$Sepal.Width)
median(iris$Sepal.Width)
min(iris$Sepal.Width)
max(iris$Sepal.Width)
sd(iris$Sepal.Width)
b = aggregate( x=iris$Sepal.Width, by=list( iris$Species), FUN=median )
b = b[ order( a$x, decreasing=TRUE), ]
b