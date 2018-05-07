## If we base our model solely on voter turnout, it predicts Trump every
## time. 
## 
## I'm not sure if that's because voter turnout helped him or
## because my RStudio skills are nill.

require(dplyr)
require(MASS)

# Importing dataset into RStudio
project <- "https://data.world/ncaldw01/election-results"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM ElectionsData2"),
  dataset = project
) %>%
  dplyr::mutate(
    winner=ifelse(pop_vote_winner=="Trump",1,0),
    turnout=votes/total_population)
attach(df)

train = df[1:34,]
test = df[35:50,]

# GLM, LDA, QDA, KNN
glm.fit = glm(winner~turnout, data=train, family=binomial)
lda.fit = lda(pop_vote_winner~turnout, data=train)
qda.fit = qda(pop_vote_winner~turnout, data=train)

glm.preds = predict(glm.fit,newdata=test,type="response")
glm.preds = ifelse(glm.preds>0.5,"Trump","Clinton") %>% data.frame()
lda.preds=predict(lda.fit,test) %>% data.frame()
qda.preds=predict(qda.fit,test) %>% data.frame()
knn.preds = knn(
  cbind(train$turnout),
  cbind(test$turnout),
  pop_vote_winner[state<'O'], # this grabs same # of rows as train set
  k=1)

table(glm.preds$., test$pop_vote_winner)
table(lda.preds$class, test$pop_vote_winner)
table(qda.preds$class,test$pop_vote_winner)
table(knn.preds, pop_vote_winner[state>='O'])

mean(glm.preds$.==test$pop_vote_winner) # 0.56
mean(lda.preds$class==test$pop_vote_winner) # 0.5625
mean(qda.preds$class==test$pop_vote_winner) # 0.4375
mean(knn.preds==pop_vote_winner[state>='O']) # 0.5

# Those numbers aren't good, and they don't seem to tell us much of a/t
# interesting. However, looking at each set of predictions on their own,
# we find something interesting: almost without exception, they predict
# that Trump will win every state. I wondered what the mean variance for
# LDA and QDA.

mean(lda.preds$posterior.Clinton-(test$votes16_clintonh/test$votes))
  # Mean variance: -0.05
mean(qda.preds$posterior.Clinton-(test$votes16_clintonh/test$votes))
  # Mean variance: -0.08

# That is a pretty significant variance, suggesting the models are 
# definitely not doing a good job. 