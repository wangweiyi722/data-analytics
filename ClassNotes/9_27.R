## Linear Discriminant Analysis
require(MASS)
require(ISLR)
require(tidyverse)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit

# Prior probability SQL: select direction, count(*) / 998 from smarket where year < 2005 group by direction
# Group mean SQL: select direction, avg(lag2) from smarket where year < 2005 group by direction
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
head(Smarket.2005)
#      Year  Lag1   Lag2   Lag3   Lag4   Lag5  Volume  Today      Direction
# 999  2005 -0.134  0.008 -0.007  0.715 -0.431 0.7869 -0.812      Down
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
# Posterior up is prior probability of being up
df = data.frame(lda.pred)
ggplot(df) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class)
ggplot(df) + geom_boxplot(mapping = aes(x=class, y=LD1))
table(lda.pred$class,Smarket.2005$Direction) # Possible quiz questions, real value is col, row is predicted
mean(lda.pred$class==Smarket.2005$Direction)

# Now let's look at an example that has 3 Classes in Y. To do this, see the example at the end of ?lda.
?lda
# What is iris?
?iris
# Here's the example:
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("s","c","v"), rep(50,3)))
train <- sample(1:150, 75)
table(Iris$Sp[train])
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
z
iris.predict = predict(z, Iris[-train, ]) # $class
data.frame(iris.predict)[1:5,]
df = data.frame(iris.predict)
ggplot(df) + geom_histogram(mapping = aes(x=x.LD1), color="blue") + geom_histogram(mapping = aes(x=x.LD2), color="red")+ facet_wrap(~ class)
ggplot(df) + geom_boxplot(mapping = aes(x=class, y=x.LD1), color="blue") + geom_boxplot(mapping = aes(x=class, y=x.LD2), color="red")
ggplot(df) + geom_point(mapping = aes(x=x.LD1, y=x.LD2, colour=class))
table(iris.predict$class, Iris[-train, ]$Sp)
mean(iris.predict$class == Iris[-train, ]$Sp)
