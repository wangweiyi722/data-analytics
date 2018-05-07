library(ISLR)
summary(Hitters)

# Examples from Chapter 6
Hitters=na.omit(Hitters)
with(Hitters,sum(is.na(Salary)))

library(pls)
set.seed(2)
pca.fit = pcr(Salary~., data=Hitters, scale=TRUE)
pca.fit
summary(pca.fit)
validationplot(pca.fit, val.type="MSEP")
biplot(pca.fit, scale=.5)

library(tidyverse)
H = Hitters %>% dplyr::select(-League, -NewLeague, -Division)
pca.fit = prcomp(H, scale=TRUE)
pca.fit
summary(pca.fit)
validationplot(pca.fit, val.type="MSEP")
biplot(pca.fit, scale=.5)

# Examples from Chapter 10
dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2, var)
pca.out=prcomp(USArrests, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)