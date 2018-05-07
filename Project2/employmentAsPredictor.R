## Modeling winner against the six profession variables (not counting
##   unemployed)
## QDA model produces 86% accurate predictions

require(class)

# Importing dataset into RStudio
project <- "https://data.world/ncaldw01/election-results"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM ElectionsData2"),
  dataset = project
)
View(df)
sdf = df %>% dplyr::select(., -matches("16_"), -state, -total_population, -votes) %>%
  dplyr::mutate(winner=ifelse(pop_vote_winner=="Trump", 1, 0)) %>%
  dplyr::select(., -pop_vote_winner)
sdf %>% cor() %>% data.frame() %>% View()
# Adding winner
df = dplyr::mutate(df, winner=ifelse(pop_vote_winner=="Trump", 1, 0))
attach(df)

# Making training and testing dataframes
train = df[1:25,]
test = df[26:50,]

# LDA model
lda.fit = lda(winner~
                 management_professional_and_related_occupations +
                 service_occupations +                                      
                 sales_and_office_occupations +
                 farming_fishing_and_forestry_occupations +               
                 construction_extraction_maintenance_and_repair_occupations +
                 production_transportation_and_material_moving_occupations,
               data=train)
lda.preds=predict(lda.fit,test) %>% data.frame()
mean(lda.preds$class==test$winner) # 76%

qda.fit = qda(winner~
                management_professional_and_related_occupations +
                service_occupations +                                      
                sales_and_office_occupations +
                farming_fishing_and_forestry_occupations +               
                construction_extraction_maintenance_and_repair_occupations +
                production_transportation_and_material_moving_occupations,
              data=train)
qda.preds=predict(qda.fit,test) %>% data.frame()
mean(qda.preds$class==test$winner) # 86%

# KNN
Xlag=cbind(management_professional_and_related_occupations,
           service_occupations,
           sales_and_office_occupations,
           farming_fishing_and_forestry_occupations,
           construction_extraction_maintenance_and_repair_occupations,
           production_transportation_and_material_moving_occupations)
train_cond=state<'L'
knn.pred=knn(Xlag[train_cond,],Xlag[!train_cond,],pop_vote_winner[train_cond],k=1)
table(knn.pred,pop_vote_winner[!train_cond])
mean(knn.pred==pop_vote_winner[!train_cond]) # 69.7%