# Importing dataset into RStudio
project <- "https://data.world/ncaldw01/election-results"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM ElectionsData2"),
  dataset = project
) %>%
  dplyr::mutate(winner=ifelse(pop_vote_winner=="Trump",1,0))
attach(df)

train = df[1:34,]
test = df[35:50,]

# Found correlations
data.frame("winner") %>%
  dplyr::mutate(
    mgmt_prof=cor(winner,management_professional_and_related_occupations),
    service=cor(winner,service_occupations),
    sales_office=cor(winner,sales_and_office_occupations)
  ) %>%
  View()

# Put here the resulting table w/ correlations
# Each of those is negatively correlated w/ winner, i.e. they made a
#   state more likely to go for Clinton. I wanted to make a model.

# GLM, LDA, QDA, KNN
glm.fit = glm(winner~
                management_professional_and_related_occupations+
                service_occupations+
                sales_and_office_occupations,
              data=train,
              family=binomial)
lda.fit = lda(pop_vote_winner~
                management_professional_and_related_occupations+
                service_occupations+
                sales_and_office_occupations,
              data=train)
qda.fit = qda(pop_vote_winner~
                management_professional_and_related_occupations+
                service_occupations+
                sales_and_office_occupations,
              data=train)

glm.preds = predict(glm.fit,newdata=test,type="response")
glm.preds = ifelse(glm.preds>0.5,"Trump","Clinton") %>% data.frame()
lda.preds=predict(lda.fit,test) %>% data.frame()
qda.preds=predict(qda.fit,test) %>% data.frame()
knn.preds = knn(
  cbind(
    train$management_professional_and_related_occupations, 
    train$service_occupations,
    train$sales_and_office_occupations),
  cbind(
    test$management_professional_and_related_occupations, 
    test$service_occupations,
    test$sales_and_office_occupations),
  pop_vote_winner[state<'O'], # this grabs same # of rows as train set
  k=1)

table(glm.preds$., test$pop_vote_winner)
table(lda.preds$class, test$pop_vote_winner)
table(qda.preds$class,test$pop_vote_winner)
table(knn.preds, pop_vote_winner[state>='O'])

mean(glm.preds$.==test$pop_vote_winner) # 0.81
mean(lda.preds$class==test$pop_vote_winner) # 0.875
mean(qda.preds$class==test$pop_vote_winner) # 0.8125
mean(knn.preds==pop_vote_winner[state>='O']) # 0.5