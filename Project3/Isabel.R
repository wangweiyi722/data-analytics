require(MASS)
require(ISLR)
require(tidyverse)
library(dplyr)

project<- "https://data.world/isabelcachola/f-17-eda-project-3"
df <- read.csv("https://query.data.world/s/0uetDd56RIiSaweOGdPqWuy2Idz44K", header=TRUE, stringsAsFactors=FALSE)
names(df)

sdf<- subset(df, country=='US')
sdf<- subset(sdf, state=='successful'|state=='failed')

set.seed(11)

train <- sdf[sample(nrow(sdf), 8421),] # 70% of the data to train
test <- subset(sdf, !is.element(sdf$X, train$X))

################ Insight 1 ##########################
lda_func <-function(lda.fit){
  print(lda.fit)
  
  lda.pred = predict(lda.fit, test)
  df.pred = data.frame(lda.pred)
  test$lda_class = lda.pred$class
  print(ggplot(df.pred) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class))
  print(ggplot(df.pred) + geom_boxplot(mapping = aes(x=class, y=LD1)))
  print(table(lda.pred$class,test$state))
  print(paste('Percent correct:', toString(mean(lda.pred$class==test$state))))
  print(paste('Percent incorrect:', toString(mean(lda.pred$class!=test$state))))
  
}

attach(sdf)
convert_weekday <- function(num){
  if (num=='Monday'){return(1)}
  if (num=='Tuesday'){return(2)}
  if (num=='Wednesday'){return(3)}
  if (num=='Thursday'){return(4)}
  if (num=='Friday'){return(5)}
  if (num=='Saturday'){return(6)}
  if (num=='Sunday'){return(7)}
  
}
sdf<- dplyr::mutate(sdf, day_launched=launched_at_weekday) 
sdf$day_launched <- recode(sdf$day_launched, 
                        "Sunday"=0,
                        "Monday"=1,
                        "Tuesday"=2,
                        "Wednesday"=3,
                        "Thursday"=4,
                        "Friday"=5,
                        "Saturday"=6)

train <- sdf[sample(nrow(sdf), 8421),] # 70% of the data to train
test <- subset(sdf, !is.element(sdf$X, train$X))

sdf$launched_at_weekday <- as.character(sdf$launched_at_weekday)
sdf$launched_at_weekday <- factor(sdf$launched_at_weekday, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))


ggplot(data=sdf, mapping=aes(sdf$launched_at_weekday)) + 
  geom_bar(aes(fill=state)) + 
  geom_text(stat='count',aes(label=..count..),vjust=-1) 

lda_func(lda(state~day_launched, data=train))
lda.pred = data.frame(predict(lda(state~day_launched, data=train),test))
table(lda.pred$class,test$state)
paste('Percent correct:', toString(mean(lda.pred$class==test$state)))
paste('Percent incorrect:', toString(mean(lda.pred$class!=test$state)))
test$posterior_successful = lda.pred$posterior.successful
ggplot(data=test, mapping = aes(x=state,y=posterior_successful)) + geom_boxplot()
test$posterior_failed = lda.pred$posterior.failed
ggplot(data=test, mapping = aes(x=state,y=posterior_failed)) + geom_boxplot()


###################### Insight 2 ########################

sdf_select <-dplyr::mutate(sdf, net_pledged = pledged-goal,
                           percent_pledged = pledged/goal,
                           diff_create_launch=as.numeric(as.Date(as.character(sdf$launched_at), format='%m/%d/%Y') - as.Date(as.character(sdf$created_at), format='%m/%d/%Y')))
sdf_select <- dplyr::select(sdf_select, goal, backers_count, name_len, blurb_len,created_at_day,launched_at_day, net_pledged, diff_create_launch)

#pairs(sdf_select)
library(leaps)
regfit.full=regsubsets(net_pledged~.,data=sdf_select)
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,which.min(reg.summary$cp))



lm.fit = lm(net_pledged~goal+backers_count+name_len+diff_create_launch, data = sdf_select)
summary(lm.fit)

##################### Insight 4 ##################################
sdf_select <-dplyr::mutate(sdf, net_pledged = pledged-goal,
                           percent_pledged = pledged/goal,
                           diff_create_launch=as.numeric(as.Date(as.character(sdf$launched_at), format='%m/%d/%Y') - as.Date(as.character(sdf$created_at), format='%m/%d/%Y')))
sdf_select <- dplyr::select(sdf_select, goal, backers_count, name_len, blurb_len,created_at_day,launched_at_day, net_pledged, diff_create_launch,percent_pledged)
pairs(sdf_select)
library(leaps)
regfit.full=regsubsets(percent_pledged~.,data=sdf_select,method='forward')
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,1)

lm.fit = lm(percent_pledged~diff_create_launch, data = sdf_select)
summary(lm.fit)

ggplot(data=sdf_select, mapping = aes(x=diff_create_launch,y=percent_pledged)) + geom_point()
ggplot(data=sdf_select, mapping = aes(percent_pledged)) + geom_histogram(bins = 50)
summary(sdf_select$percent_pledged)

ggplot(data=dplyr::filter(sdf_select, percent_pledged < 3*sd(percent_pledged)), mapping = aes(percent_pledged)) + geom_histogram()
ggplot(data=dplyr::filter(sdf_select, percent_pledged <= 1.10), mapping = aes(percent_pledged)) + geom_histogram(bins=30)
ggplot(data=dplyr::filter(sdf_select, percent_pledged <= .9&percent_pledged>0.1), mapping = aes(percent_pledged)) + geom_histogram(bins=100) + geom_density(colour='red')

test <-subset(sdf_select$percent_pledged,sdf_select$percent_pledged <= .9&sdf_select$percent_pledged >= .1)
counts <- hist(test, freq = FALSE, breaks = 100,prob=TRUE)$counts
lambda <- 1/(mean(test))
curve(lambda*exp(-lambda*x), col = "blue", add = TRUE)
#curve(4*exp(-lambda*x), col = "red", add = TRUE)
#curve(1.5*dexp(x, rate = 4.5), col = "blue", add = TRUE)
#function1 <- function(x){return (1.5*dexp(x, rate = 4.5))}
"
mse <- function(x1,x2){
  error = 0
  for(i in range(length(x1))){
    error = error + ((x1[i] - x2[i])^2)
  }
  error = (error)/length(x1)
  return(error)
}
mse(counts,function1(test))"


sdf1 <- dplyr::filter(sdf_select, percent_pledged <= 1.10&percent_pledged >= 0.9)
var(sdf1$percent_pledged)
ggplot(data=sdf1,mapping = aes(percent_pledged)) + geom_histogram(bins=30,colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")
sdf2 <- dplyr::filter(sdf_select, percent_pledged <= 1.10&percent_pledged >= 1)
lambda <- 1/mean(sdf2$percent_pledged)
ggplot(data=sdf2,mapping = aes(percent_pledged)) + geom_histogram(bins=100,colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666")


ggplot(data=sdf1, mapping = aes(x=diff_create_launch,y=percent_pledged)) + geom_point()


##################### Insight 5 ##################################
sdf_select <-dplyr::mutate(sdf, net_pledged = pledged-goal,
                           percent_pledged = pledged/goal,
                           diff_create_launch=as.numeric(as.Date(as.character(sdf$launched_at), format='%m/%d/%Y') - as.Date(as.character(sdf$created_at), format='%m/%d/%Y')))
sdf_select <- dplyr::filter(sdf_select, percent_pledged >= 0.1 &percent_pledged <= 0.9) 
lambda <- 1/mean(sdf_select$percent_pledged)
sdf_select <- dplyr:: mutate(sdf_select, log_percent_pledged = log(percent_pledged, base=lambda)) 
sdf_select <- dplyr::select(sdf_select, backers_count, name_len, blurb_len,created_at_day,launched_at_day, diff_create_launch,log_percent_pledged)

regfit.full=regsubsets(log_percent_pledged~.,data=sdf_select)
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,1)

lm.fit = lm(log_percent_pledged~backers_count, data = sdf_select)
summary(lm.fit)
ggplot(data = sdf_select, mapping = aes(x=backers_count,y=log_percent_pledged)) + geom_point()
summary(sdf_select$backers_count)

sdf1 <- dplyr::filter(sdf_select,backers_count > 14 & backers_count < 78)
ggplot(data = sdf1, mapping = aes(x=backers_count,y=log_percent_pledged)) + geom_point()
cor(sdf1)

ggplot(data=sdf1, mapping = aes(backers_count)) + geom_histogram(bins=50) + geom_density(colour = 'red')


############# Insight 6 ################

#K Selection Using Validation
set.seed(10)
library(class)
train <- sample(seq_len(nrow(sdf)), size = .7*nrow(sdf))
val.percents=rep(NA,10)
for(i in 1:10){
  knn.pred = knn(cbind(sdf[train,]$backers_count),cbind(sdf[-train,]$backers_count),sdf[train,]$state,k=i)
  val.percents[i]=mean(knn.pred==sdf[-train,]$state)
}
which.max(val.percents)
plot(val.percents,ylab="Percent Correct", pch=20,type="b")

knn.pred = knn(cbind(sdf[train,]$backers_count),cbind(sdf[-train,]$backers_count),sdf[train,]$state,k=which.max(val.percents))
#table(knn.pred,sdf[-train,]$state)
mean(knn.pred==sdf[-train,]$state)
summary(val.percents)

ggplot(data=sdf, mapping=aes(x=state, y=backers_count)) + geom_boxplot()
sdf1 <- dplyr::filter(sdf,backers_count > 14 & backers_count < 78)
ggplot(data=sdf1, mapping=aes(x=state, y=backers_count)) + geom_boxplot()


#CV
folds=sample(rep(1:10,length=nrow(sdf)))
cv.percents=matrix(NA,10,10)
for(j in 1:10){
  for(i in 1:5){
    pred = knn(cbind(sdf[-(folds==j),]$backers_count),cbind(sdf[(folds==j),]$backers_count),sdf[-(folds==j),]$state,k=i)
    cv.percents[j,i]=mean(knn.pred==sdf[folds==j]$state)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")
lm.fit1 <- lm(log_percent_pledged~backers_count+created_at_day, data=sdf_select)
summary(lm.fit1)

############### Insight 7 ###################
sdf_class <- dplyr::mutate(sdf, net_pledged = pledged-goal,
                                percent_pledged = pledged/goal,
                                diff_create_launch=as.numeric(as.Date(as.character(sdf$launched_at), format='%m/%d/%Y') - as.Date(as.character(sdf$created_at), format='%m/%d/%Y')))

sdf_class <- dplyr::mutate(sdf_class, class_percent = ifelse(percent_pledged < .5, 'less than 50 percent',
                                                             ifelse(percent_pledged >= .5 & percent_pledged < 1, '50 to 100 percent',
                                                             'reached goal')))
set.seed(5)
library(splitstackshape)
train <- stratified(sdf_class, c('class_percent'), size=.7)
test <- subset(sdf_class, !is.element(sdf_class$X,train$X))

qda.fit <- qda(class_percent~backers_count, data=train)
qda.fit
qda.pred = data.frame(predict(qda.fit, test))
mean(qda.pred$class==test$class_percent)

ggplot(data = subset(sdf_class,sdf_class$backers_count<5000), mapping=aes(x=class_percent,y=backers_count)) + geom_boxplot()
