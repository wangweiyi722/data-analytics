require(dplyr)
require(data.world)
setwd("C:/Users/Weiyi/OneDrive/Documents/CS 329E")
df <- read.csv("ElectionsData.csv")

project <- "https://data.world/ncaldw01/electionsdata"

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Ondhbmd3ZWl5aTcyMiIsImlzcyI6ImFnZW50Ondhbmd3ZWl5aTcyMjo6OGRmYzgxYmYtOWNjMS00OWYzLWJkMzEtOTBjZjIxMjE0YTY2IiwiaWF0IjoxNDg0Njk3NDM5LCJyb2xlIjpbInVzZXJfYXBpX3JlYWQiLCJ1c2VyX2FwaV93cml0ZSJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.DA9rL8enFktjNWwhf141FieMdhmP5mh4bEix0LBGut5cz0Nce5hVQ0-y3mYC5A9JnWpRZmGusmpmRsLr7xzBPg"))

df <- data.world::query(
  data.world::qry_sql("SELECT * FROM electionsdata2"),
  dataset = project
)


attach(df)
sdf = dplyr::select(df, state, votes, votes16_trumpd, votes16_clintonh, pop_vote_winner, at_least_bachelor_s_degree, white_not_latino_population, african_american_population, native_american_population, asian_american_population, population_some_other_race_or_races, latino_population)

pairs(sdf)
sdf = dplyr::mutate(sdf,perc_minority =  african_american_population+ native_american_population+ asian_american_population+ population_some_other_race_or_races+ latino_population)%>%arrange(votes)



# Create a training data set using the 25 states with the lowest number of votes.
training = sdf%>% dplyr::filter(votes<2000000)
testing = sdf%>% dplyr::filter(votes>2000000)

# Use the perc_minority column from the testing data to make a lda model predicting whether Trump or Clinton won that state
election_lda = lda(pop_vote_winner~perc_minority,data = training)
plot(election_lda)
data.frame(election_lda)[1:5,]
election_lda_pred = predict(election_lda,testing)
table(election_lda_pred$class,testing$pop_vote_winner)


# Use different testing and training data
training = sdf%>% dplyr::filter(state<"Montana")
testing = sdf%>% dplyr::filter(state>"Missouri")

election_lda = lda(pop_vote_winner~perc_minority,data = training)
plot(election_lda)
data.frame(election_lda)[1:5,]
election_lda_pred = predict(election_lda,testing)
table(election_lda_pred$class,testing$pop_vote_winner)

#--------------------------------------------------------------------------------------------------------------
# Logistic Regression

# Change Trump to 1 and Hillary to 0
sdf = dplyr::mutate(sdf,trump1hillary0 = if_else(votes16_trumpd>votes16_clintonh,1,0))

training = sdf%>% dplyr::filter(state>"Montana")
testing = sdf%>% dplyr::filter(state>"Missouri")

perc_min_logis_fit=glm(trump1hillary0~perc_minority, data = training, family = binomial)
perc_min_logis_probs=predict(perc_min_logis_fit,newdata=testing,type="response") 
perc_min_logis_pred=ifelse(perc_min_logis_probs >0.55,"Trump","Clinton")
winner=testing$pop_vote_winner
table(perc_min_logis_pred,winner)
mean(perc_min_logis_pred==winner)

#------------------------------------------------------------------------------------------------------------
# Quadratic Discriminate Analysis
election_qda = qda(pop_vote_winner~perc_minority,data = training)
ggplot(election_qda)
election_qda_pred = predict(election_qda,testing)
table(election_qda_pred$class,testing$pop_vote_winner)

#------------------------------------------------------------------------------------------------------------
# K-nearest neighbor
library(class)
?knn
training = sdf%>% dplyr::filter(state<"Montana")
testing = sdf%>% dplyr::filter(state>"Missouri")
perc_min_df = dplyr::select(sdf,state,perc_minority)
perc_min_knn_pred=knn(data.frame(training$perc_minority),data.frame(testing$perc_minority),training$pop_vote_winner,k=1)
table(perc_min_knn_pred,training$pop_vote_winner)
mean(perc_min_knn_pred==training$pop_vote_winner)
