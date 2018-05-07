require(dplyr)
require(MASS)
require(ggplot2)

# Importing dataset into RStudio
project <- "https://data.world/ncaldw01/election-results"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM ElectionsData2"),
  dataset = project
)
attach(df)

# Paring out useless variables
sdf = df %>% dplyr::select(., -matches("16_"), -state, -total_population, -votes) %>%
  dplyr::mutate(winner=ifelse(pop_vote_winner=="Trump", 1, 0)) %>%
  dplyr::select(., -pop_vote_winner)
  # I had to get rid of pop_vote_winner and use winner instead b/c it
  # wouldn't let me make a model predicting strings for some reason
names(sdf)

# Making test and train data
train = sdf[1:25,]
test = sdf[26:50,]

# Just fitting on education. Cut out bachelor's because it's closely correlated w/ 
# grad.
lda.fit3 = lda(
  winner ~ 
    graduate_degree + 
    less_than_high_school + 
    at_least_high_school_diploma,
  data=train)
lda.preds=predict(lda.fit3,test) %>% data.frame()
mean(lda.preds$class==test$winner)
lda.fit3
ggplot(lda.preds) +
  geom_histogram(mapping=aes(x=LD1)) +
  facet_wrap(~class)
ggplot(lda.preds) +
  geom_boxplot(mapping=aes(x=class,y=LD1))