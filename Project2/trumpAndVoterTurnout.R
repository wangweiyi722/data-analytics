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
)
attach(df)

# Adding voter turnout
sdf = df %>%
  dplyr::mutate(
    winner=ifelse(pop_vote_winner=="Trump", 1, 0), 
    turnout=votes/total_population)
names(sdf)

# Making our training and testing data
train1 = sdf[1:25,]
test1 = sdf[26:50,]

# First, trying glm(). -> 0.6
glm.linearfit = glm(winner~turnout, data=train1)
glm.binomialfit = glm(winner~turnout, data=train1, family=binomial)

glm.linearpreds = predict(glm.linearfit,newdata=test1,type="response"); glm.linearpreds = ifelse(glm.linearpreds>0.5,1,0) %>%
  data.frame()
glm.binomialpreds = predict(glm.binomialfit,newdata=test1,type="response"); glm.binomialpreds = ifelse(glm.binomialpreds>0.5,1,0) %>%
  data.frame()
mean(glm.linearpreds$.==test1$winner) #.6: not terrible. Better than Smarket
mean(glm.binomialpreds$.==test1$winner)
table(glm.linearpreds$., test1$winner)
table(glm.binomialpreds$., test1$winner)
  # It only ever predicts Trump!

# Next, trying lda(). Also -> 0.6
lda.fit = lda(winner~turnout, data=train1)
lda.preds=predict(lda.fit,test1) %>% data.frame()
mean(lda.preds$class==test1$winner)  # Again, 0.6.
table(lda.preds$class, test1$winner)
  # Again, only ever predicts Trump!