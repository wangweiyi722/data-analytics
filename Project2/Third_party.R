require(MASS)
require(ISLR)
require(tidyverse)
library(dplyr)
require(class)

project<- "https://data.world/ncaldw01/election-results"
df <- read.csv("https://query.data.world/s/9AePdX0-UXY6R90eqhjXNuTAA1imZ9", header=TRUE, stringsAsFactors=FALSE);
names(df)

sdf = dplyr::select(df, rep16_frac, State, votes, votes16_trumpd, votes16_clintonh, votes16_johnsong, votes16_steinj, pop_vote_winner)
sdf = sdf[complete.cases(sdf),] # Remove rows with NA
sdf = dplyr::mutate(sdf,perc_third_party = (votes - votes16_trumpd - votes16_clintonh)/votes) # Add percent of third party votes


###################### LDA ###############################

# Partitions data set randomly into 30 states for a training set and 20 states for a testing set
df_train <- sdf[sample(nrow(sdf), 25),]
df_test <- subset(sdf, !is.element(sdf$State, df_train$State))

# Function that prints histogram, boxplit, confusion matrix, percent predicted correctly,
# percent predicted incorrectly, and states predicted incorrectly
# Takes lda fit as input
lda_func <-function(lda.fit){
  print(lda.fit)
  
  lda.pred = predict(lda.fit, df_test)
  df.pred = data.frame(lda.pred)
  df_test$lda_class = lda.pred$class
  print(ggplot(df.pred) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class))
  print(ggplot(df.pred) + geom_boxplot(mapping = aes(x=class, y=LD1)))
  print(table(lda.pred$class,df_test$pop_vote_winner))
  print(paste('Percent correct:', toString(mean(lda.pred$class==df_test$pop_vote_winner))))
  print(paste('Percent incorrect:', toString(mean(lda.pred$class!=df_test$pop_vote_winner))))
  subset(df_test, lda.pred$class!=df_test$pop_vote_winner)$State
  
}


# Looks at how third party votes affected the election
lda_func(lda(pop_vote_winner~perc_third_party, data=df_train))

#Polynomial attempt
percent_incorrect=rep(0,5)
degree=1:5
for(d in degree){
  fit=lda(pop_vote_winner~poly(perc_third_party,d), data=df_train)
  fit.pred = data.frame(predict(fit, df_test))
  percent_incorrect[d]=mean(fit.pred$class!=df_test$pop_vote_winner)
}
plot(degree,percent_incorrect,type="b")
percent_incorrect


# Take random sample 5 times and average percent correct
percent_correct_func <- function(fit,df_test){
  fit.pred = data.frame(predict(fit, df_test))
  return(mean(fit.pred$class==df_test$pop_vote_winner))
}

df_percent_correct = data.frame(avg_percent_correct = rep(0,4))
percent_correct_redos=rep(0,5)
for (i in 1:5){
  # Redoes random partition
  df_train_redo <- sdf[sample(nrow(sdf), 25),]
  df_test_redo <- subset(sdf, !is.element(sdf$State, df_train_redo$State))
  
  # Uses LDA on new partition
  fit=lda(pop_vote_winner~perc_third_party, data=df_train_redo)
  percent_correct_redos[i] = percent_correct_func(fit, df_test_redo)
}
mean(percent_correct_redos)


sdf = dplyr::mutate(sdf,perc_johnson = (votes16_johnsong/votes))
sdf = dplyr::mutate(sdf,perc_stein = (votes16_steinj/votes))
df_train <- sdf[sample(nrow(sdf), 25),]
df_test <- subset(sdf, !is.element(sdf$State, df_train$State))
lda_func(lda(pop_vote_winner~perc_stein, data=df_train))
lda_func(lda(pop_vote_winner~perc_johnson, data=df_train))

sdf$fCategory <- factor(sdf$pop_vote_winner)
ggplot(sdf,aes(x=rep16_frac,y=perc_stein, col=fCategory)) + geom_point()

sdf = dplyr::mutate(sdf,bin_winner = ifelse(sdf$pop_vote_winner=='Clinton',1,0))
ggplot(sdf,aes(x=pop_vote_winner,y=perc_stein,col=sdf$fCategory)) + 
      geom_boxplot(colour='black') + 
      geom_jitter(position=position_jitter(0))


######################### QDA ################################

df_train <- sdf[sample(nrow(sdf), 25),]
df_test <- subset(sdf, !is.element(sdf$State, df_train$State))

qda_func <-function(qda.fit){
  print(qda.fit)
  
  qda.pred = predict(qda.fit, df_test)
  df.pred = data.frame(qda.pred)
  print(table(qda.pred$class,df_test$pop_vote_winner))
  print(paste('Percent correct:', toString(mean(qda.pred$class==df_test$pop_vote_winner))))
  print(paste('Percent incorrect:', toString(mean(qda.pred$class!=df_test$pop_vote_winner))))
  print('States predicted incorrectly:')
  print(subset(df_test, qda.pred$class!=df_test$pop_vote_winner)$State)
  
}

qda_func(qda(pop_vote_winner~perc_third_party, data=df_train))

qda_func(qda(pop_vote_winner~perc_stein, data=df_train))
qda_func(qda(pop_vote_winner~perc_johnson, data=df_train))

# Takes average percent correct after reparticioning 5 times
df_percent_correct = data.frame(avg_percent_correct = rep(0,4))
percent_correct_redos=rep(0,5)
for (i in 1:5){
  # Redoes random partition
  df_train_redo <- sdf[sample(nrow(sdf), 25),]
  df_test_redo <- subset(sdf, !is.element(sdf$State, df_train_redo$State))
  
  # Uses QDA on new partition
  fit=qda(pop_vote_winner~perc_stein, data=df_train_redo)
  percent_correct_redos[i] = percent_correct_func(fit, df_test_redo)
}
mean(percent_correct_redos)

########################### KNN ############################
df_train <- sdf[sample(nrow(sdf), 22),]
df_test <- subset(sdf, !is.element(sdf$State, df_train_redo$State))

knn_func <- function(knn.pred){
  print(table(knn.pred,df_test$pop_vote_winner))
  print(paste('Percent correct:', toString(mean(knn.pred==df_test$pop_vote_winner))))
  print(paste('Percent incorrect:', toString(mean(knn.pred!=df_test$pop_vote_winner))))
  subset(df_test, knn.pred !=df_test$pop_vote_winner)$State
}

knn_func(knn.pred = knn(cbind(df_train$perc_stein),cbind(df_test$perc_stein),df_train$pop_vote_winner,k=5))
knn_func(knn.pred = knn(cbind(df_train$perc_stein),cbind(df_test$perc_johnson),df_train$pop_vote_winner,k=5))
knn_func(knn.pred = knn(cbind(df_train$perc_stein),cbind(df_test$perc_third_party),df_train$pop_vote_winner,k=6))

# Test for optimal k's
for (i in 1:20){
  print(paste('i = ', toString(i)))
  knn_func(knn.pred = knn(cbind(df_train$perc_stein),cbind(df_test$perc_stein),df_train$pop_vote_winner,k=i))
}

# k = 5

############################ Logistic Regression #################################

logistic_func <-function(glm.fit,bound){
  glm.probs=predict(glm.fit,newdata=df_test,type="response") 
  abc <- df_test %>% dplyr::mutate(prior_probs = glm.probs)
  print('Summary of prior probabilities for class Clinton')
  print(summary(subset(abc,pop_vote_winner=='Clinton')$prior_probs))
  print('Summary of prior probabilities for class Trump')
  print(summary(subset(abc,pop_vote_winner=='Trump')$prior_probs))
  glm.pred=ifelse(glm.probs > bound,"Clinton","Trump")
  print(table(glm.pred,df_test$bin_winner))
  print(paste('Percent correct:', toString(mean(glm.pred==df_test$pop_vote_winner))))
  print('States predicted incorrectly:')
  print(subset(df_test, glm.pred!=df_test$pop_vote_winner)$State)
}

logistic_func(glm(bin_winner~perc_stein, data=df_train,family=binomial),0.6)
#print(ggplot(data=abc, mapping = aes(x=pop_vote_winner,y=prior_probs)) + geom_boxplot())
logistic_func(glm(bin_winner~perc_johnson, data=df_train,family=binomial),0.8)
logistic_func(glm(bin_winner~perc_third_party, data=df_train,family=binomial),0.4)

############### What if everyone who voted for Stein voted for Hillary? ####################
count(sdf, sdf$pop_vote_winner =='Clinton')
df2 <- df[complete.cases(df),]
attach(df2)
df2 <- dplyr:: mutate(df2, new_clinton_stein = (votes16_clintonh+votes16_steinj))
df2 <- dplyr:: mutate(df2, new_winner_stein = ifelse(new_clinton_stein > votes16_trumpd, 'Clinton','Trump'))
df2 <- dplyr:: mutate(df2, new_clinton_all = (votes16_clintonh+votes16_steinj+votes16_johnsong))
df2 <- dplyr:: mutate(df2, new_winner_all = ifelse(new_clinton_all > votes16_trumpd, 'Clinton','Trump'))
count(df2, df2$new_winner_stein =='Clinton')
count(df2, df2$new_winner_all =='Clinton')