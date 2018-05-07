require(MASS)
require(ISLR)
require(tidyverse)
library(ggplot2)
library(dplyr)

project<- "https://data.world/wangweiyi722/f-17-eda-project-4"
<<<<<<< HEAD
df_orig <- read.csv("https://query.data.world/s/hvA1ht-J0O9mzt550iY43aXKb5fGDl", header=TRUE, stringsAsFactors=FALSE)
names(df_orig)

=======
df_orig <- read.csv("https://query.data.world/s/tZl4yOP0Ui6RbVBm482-KU67IDetEk", header=TRUE, stringsAsFactors=FALSE)
names(df_orig)
>>>>>>> 2e9c8aa11e6226c8ac97cb4ee0131e7b634960ed
################################# Insght 1 ########################################
library(maps)
library(mapdata)

df_map = subset(df_orig, (df_orig$Title.1.Eligible == "Yes")|(df_orig$Title.1.Eligible == "No"))
<<<<<<< HEAD

df_map <- dplyr::mutate(df_map, factored_stats = factor(df_map$Title.1.Eligible, levels = c("Yes","No")))
=======
>>>>>>> 2e9c8aa11e6226c8ac97cb4ee0131e7b634960ed

### Texas ###
df_map_texas = subset(df_map, df_map$State == "Texas")

states <- map_data("state")
tx_df <- subset(states, region == "texas")
tx_base <- ggplot(data = tx_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "black")

df_map_texas_eligible <- dplyr:: filter(df_map_texas, df_map_texas$Title.1.Eligible=="Yes")
df_map_texas_not_eligible <- subset(df_map_texas, df_map_texas$Title.1.Eligible=="No")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

tx_base +
  geom_point(data = df_map_texas_eligible, mapping = aes(x=Longitude,y=Latitude,colour='Title I Eligible')) +
  geom_point(data = df_map_texas_not_eligible,mapping = aes(x=Longitude,y=Latitude,colour='Not Title I Eligible')) + 
  geom_point(mapping = aes(x=-97.743061,y=30.267153,colour = 'Major Cities'),size = 5,shape = 18)+
  geom_point(mapping = aes(x=-96.796988,y=32.776664,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-95.369803,y=29.760427,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-98.493628,y=29.424122,colour = 'Major Cities'),size = 5,shape=18) + 
  ditch_the_axes +
  scale_color_brewer(palette="PRGn")


## US ##

usa <- map_data("usa")
us_base <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3)

df_map_main_us <- filter(df_map, (df_map$State != "Bureau of Indian Education")&(df_map$State != "Northern Marianas")&(df_map$State != "Puerto Rico")&(df_map$State != "Alaska")&(df_map$State != "Hawaii"))

df_map_main_us$State <- tolower(df_map_main_us$State)

state_eligibilty_perc <-data.frame(state = unique(df_map_main_us$State), perc = rep(0,47))
for (i in 1:47){
  state <- state_eligibilty_perc[i,]$state
  num_el <- nrow(subset(df_map_main_us, (df_map_main_us$State==state)&(df_map_main_us$Title.1.Eligible=="Yes")))
  total <- nrow(subset(df_map_main_us, (df_map_main_us$State==state)))
  percent <- num_el/total
  state_eligibilty_perc[i,]$perc <- percent
}
state_eligibilty_perc <- mutate(state_eligibilty_perc, region = state)
el_perc <- inner_join(state_eligibilty_perc, states, by = "region")

ggplot(data = el_perc) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=perc), color = "white") + 
  ggtitle("Percentage of Title I Eligibility") +
  theme_bw() +
  ditch_the_axes +
  scale_fill_gradientn(colours = rev(terrain.colors(7)),
                       breaks = c(.14, .28, .42, .56, .70, .84, 1))

el_perc[which.min(el_perc$perc),]$state
el_perc[which.max(el_perc$perc),]$state

### California ###
df_map_ca = subset(df_map, df_map$State == "California")

states <- map_data("state")
ca_df <- subset(states, region == "california")
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "black")

df_map_ca_eligible <- dplyr:: filter(df_map_ca, df_map_ca$Title.1.Eligible=="Yes")
df_map_ca_not_eligible <- subset(df_map_ca, df_map_ca$Title.1.Eligible=="No")

ca_base +
  geom_point(data = df_map_ca_eligible, mapping = aes(x=Longitude,y=Latitude,colour='Title I Eligible')) +
  geom_point(data = df_map_ca_not_eligible,mapping = aes(x=Longitude,y=Latitude,colour='Not Title I Eligible')) + 
  geom_point(mapping = aes(x=-122.419416,y=37.774929,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-117.161084,y=32.715738,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-118.243685,y=34.052234,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-121.886329,y=37.338208,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-121.494400,y=38.581572,colour = 'Major Cities'),size = 5,shape=18) +
  ditch_the_axes +
  scale_color_brewer(palette="PRGn")

### New York ###
df_map_ny = subset(df_map, df_map$State == "New York")

states <- map_data("state")
ny_df <- subset(states, region == "new york")
ny_base <- ggplot(data = ny_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "black")

df_map_ny_eligible <- dplyr:: filter(df_map_ny, df_map_ny$Title.1.Eligible=="Yes")
df_map_ny_not_eligible <- subset(df_map_ny, df_map_ny$Title.1.Eligible=="No")

ny_base +
  geom_point(data = df_map_ny_eligible, mapping = aes(x=Longitude,y=Latitude,colour='Title I Eligible')) +
  geom_point(data = df_map_ny_not_eligible,mapping = aes(x=Longitude,y=Latitude,colour='Not Title I Eligible')) + 
  geom_point(mapping = aes(x=-74.005973,y=40.712775,colour = 'Major Cities'),size = 5,shape=18) +
  ditch_the_axes +
  scale_color_brewer(palette="PRGn")

################################# Insght 2 ########################################
df_elem <- filter(df_orig, df_orig$level == 'Primary School') %>% filter(Title.1.Eligible == "Yes"|Title.1.Eligible == "No")
nrow(subset(df_elem, df_elem$Title.1.Eligible == 'Yes'))/nrow(df_elem)
df_elem <- select(df_elem, "State", "Location.City",
                    "Longitude", "Latitude", "Title.1.Eligible", "Charter", "Total.Lunch", 
                   "Full.Time.Teachers", "PreK.Offered", 
                   "member", "am", "asian", "hisp", "black", "white", "pacific", "tr", "toteth")
df_elem_test_vars <- select(df_elem, "Title.1.Eligible", "Total.Lunch", 
                            "Full.Time.Teachers", "am", "asian", "hisp", "black", "white", "pacific", "tr", "toteth")

nrow(subset(df_elem, df_elem$Title.1.Eligible=="Yes"))/nrow(df_elem) # Base line = 81 %

# Testing which variables to use
# Predicts every school as eligible
require(tree)
dim(df_elem_test_vars)
df_elem_test_vars$Title.1.Eligible <- as.factor(df_elem_test_vars$Title.1.Eligible)
elem_tree <- tree(Title.1.Eligible~., data = df_elem_test_vars)
plot(elem_tree)
text(elem_tree,pretty=0)
elem_tree

require(randomForest) 
set.seed(11)
train_vars=sample(1:nrow(df_elem_test_vars),36474) # 70% of data
rf.elem = randomForest(Title.1.Eligible ~ . ,data=df_elem_test_vars, subset=train_vars)
rf.elem
varImpPlot(rf.elem,  
           sort = T,
           main="Variable Importance",
           pch = 19,
           col = 'blue')

# Let's take the 4 most important
df_elem1 <- select(df_elem, "Title.1.Eligible", "Total.Lunch", 
                   "Full.Time.Teachers", "toteth", "white")
df_elem1$Title.1.Eligible <- as.factor(df_elem1$Title.1.Eligible)
dim(df_elem1)
train=sample(1:nrow(df_elem1),36474) # 70% of data

# Testing number of predictors sampled for spliting at each node
test.err=double(4)
for(mtry in 1:4){
  fit=randomForest(Title.1.Eligible~.,data=df_elem1,subset=train,mtry=mtry)
  #oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df_elem1[-train,])
  test.err[mtry]=mean(pred!=df_elem1[-train,]$Title.1.Eligible)
  cat(mtry," ")
}
matplot(1:mtry,test.err,pch=19,col="blue",type="b",ylab="Misclassification Error",xlab="mtry")
min_mtry<- which.min(test.err)
fit=randomForest(Title.1.Eligible~.,data=df_elem1,subset=train,mtry=min_mtry)
fit
pred=predict(fit,df_elem1[-train,])
mean(pred!=df_elem1[-train,]$Title.1.Eligible)
mean(pred==df_elem1[-train,]$Title.1.Eligible)
table(pred, df_elem1[-train,]$Title.1.Eligible)

#df_elem1$Title.1.Eligible <- as.factor(df_elem1$Title.1.Eligible)
tree.error = double(10)
numTrees <- 1:10*20
for(i in 1:10){
  fit=randomForest(Title.1.Eligible~.,data=df_elem1,subset=train,ntree=numTrees[i],mtry = min_mtry)
  pred=predict(fit,df_elem1[-train,])
  tree.error[i]=mean(pred!=df_elem1[-train,]$Title.1.Eligible)
  cat(numTrees[i]," ")
}
matplot(iter,tree.error,pch=19,col="blue",type="b",ylab="Misclassification Error",xlab = "Number of Trees")
min_tree <- numTrees[which.min(tree.error)]
fit=randomForest(Title.1.Eligible~.,data=df_elem1,subset=train,ntree=numTrees[which.min(tree.error)],mtry=min_mtry)
fit
pred=predict(fit,df_elem1[-train,])
mean(pred!=df_elem1[-train,]$Title.1.Eligible)
mean(pred==df_elem1[-train,]$Title.1.Eligible)
table(pred, df_elem1[-train,]$Title.1.Eligible)

####################################### Insight 3 #########################################

library(maps)
library(mapdata)
library(e1071)
set.seed(11)
df_svm = filter(df_orig, (Title.1.Eligible == "Yes")|(Title.1.Eligible == "No")) %>%
         filter(Location.City == 'AUSTIN' & Longitude < -95 & Latitude < 35) %>%
         select("Longitude", "Latitude", "Title.1.Eligible") %>%
         mutate(Longitude_Scaled = Longitude/3)
df_svm$Latitude <- as.numeric(df_svm$Latitude)
df_svm$Longitude <- as.numeric(df_svm$Longitude)
df_svm$Title.1.Eligible <- as.factor(df_svm$Title.1.Eligible)
ggplot(data = df_svm, mapping = aes(x=Longitude_Scaled,y=Latitude,colour=Title.1.Eligible)) + geom_point()

# Linear SVM
tuned = tune.svm(Title.1.Eligible~Longitude_Scaled+Latitude, data = df_svm, 
                 kernel = "linear",
                 cost = 1:10,
                 tunecontrol=tune.control(cross=10))

best_cost <- tuned$best.model$cost

svmfit=svm(Title.1.Eligible~Longitude_Scaled+Latitude,data=df_svm,type="C",kernel="linear",cost=best_cost)
print(svmfit)

make.grid=function(x,n=100){
  grange=apply(x,2,range)
  x1=seq(-32.67,-32.52,length=n)
  x2=seq(30.12,30.55,length=n)
  expand.grid(X1=x1,X2=x2)
}

x= cbind(df_svm$Longitude_Scaled,df_svm$Latitude)
y =df_svm$Title.1.Eligible
col_func <- function(x){  ifelse(x=="Yes","blue","red") }
col <-col_func(y)
xgrid=make.grid(x)
colnames(xgrid)[1] = "Longitude_Scaled"
colnames(xgrid)[2] = "Latitude"
ygrid=predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=col,pch=19)

tuned$best.performance



# Radial Basis
tuned = tune.svm(Title.1.Eligible~Longitude_Scaled+Latitude, data = df_svm, 
                 cost = 1:10, 
                 gamma = 1:10,
                 kernel = "radial",
                 tunecontrol=tune.control(cross=10))
best_gamma <- tuned$best.model$gamma
best_cost <- tuned$best.model$cost

svmfit=svm(Title.1.Eligible~Longitude_Scaled+Latitude,data=df_svm,type="C",kernel="radial",cost=best_cost,gamma=best_gamma)

print(svmfit)

make.grid=function(x,n=100){
  grange=apply(x,2,range)
  x1=seq(-32.67,-32.52,length=n)
  x2=seq(30.12,30.55,length=n)
  expand.grid(X1=x1,X2=x2)
}

x= cbind(df_svm$Longitude_Scaled,df_svm$Latitude)
y =df_svm$Title.1.Eligible
col_func <- function(x){  ifelse(x=="Yes","blue","red") }
col <-col_func(y)
xgrid=make.grid(x)
colnames(xgrid)[1] = "Longitude_Scaled"
colnames(xgrid)[2] = "Latitude"
ygrid=predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=col,pch=19)

tuned$best.performance

# Texas
df_svm = filter(df_orig, (Title.1.Eligible == "Yes")|(Title.1.Eligible == "No")) %>%
  filter(State == 'Texas') %>%
  select("Longitude", "Latitude", "Title.1.Eligible","Location.City") %>%
  mutate(Longitude_Scaled = Longitude/3)
df_svm$Latitude <- as.numeric(df_svm$Latitude)
df_svm$Longitude <- as.numeric(df_svm$Longitude)
df_svm$Title.1.Eligible <- as.factor(df_svm$Title.1.Eligible)
ggplot(data = df_svm, mapping = aes(x=Longitude_Scaled,y=Latitude,colour=Title.1.Eligible)) + geom_point()


# Tuning is super slow so just trust me that it returned cost=5 and gamma=10
#tuned = tune.svm(Title.1.Eligible~Longitude_Scaled+Latitude, data = df_svm, 
#                 cost = 2^2:10, 
#                 gamma = 2^2:10,
#                 kernel = "radial",
#                 tunecontrol=tune.control(cross=10))
#best_gamma <- tuned$best.model$gamma
#best_cost <- tuned$best.model$cost

#svmfit=svm(Title.1.Eligible~Longitude_Scaled+Latitude,data=df_svm,type="C",kernel="radial",cost=best_cost,gamma=best_gamma)
svmfit=svm(Title.1.Eligible~Longitude_Scaled+Latitude,data=df_svm,type="C",kernel="radial",cost=5,gamma=10)
print(svmfit)

make.grid=function(x,n=150){
  grange=apply(x,2,range)
  x1=seq(-35.54,-31.22,length=n)
  x2=seq(25.87,36.49,length=n)
  expand.grid(X1=x1,X2=x2)
}

x= cbind(df_svm$Longitude_Scaled,df_svm$Latitude)
y =df_svm$Title.1.Eligible
col_func <- function(x){  ifelse(x=="Yes","blue","red") }
col <-col_func(y)
xgrid=make.grid(x)
colnames(xgrid)[1] = "Longitude_Scaled"
colnames(xgrid)[2] = "Latitude"
ygrid=predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=col,pch=19)

tuned$best.performance

################################### Insight 4 ######################################
# Clustering 
set.seed(11)
df4 <- filter(df_orig, Title.1.Eligible == "Yes"|Title.1.Eligible == "No") %>%
                     filter(Location.City=="AUSTIN") %>%
                     mutate(stratio = Full.Time.Teachers / member, 
                            perc_white = white / member, 
                            perc_lunch = Total.Lunch / member)                      
                    
ggplot(data = df4, mapping = aes(x=perc_white,y=perc_lunch,colour=Title.1.Eligible)) + 
  geom_point() + xlab("Percentage of White Students") + ylab("Percentage of Students Qualified for Free/Reduced Lunch") +
  ggtitle("Schools in Austin")

col_func <- function(x){  ifelse(x=="Yes","blue","red") }
x <- cbind(df4$perc_white,df4$perc_lunch)
col <- col_func(df4$Title.1.Eligible)

km.out=kmeans(x,2)
km.out
km.out$cluster

cluster_col_func <- function(x){ifelse(x=="1",'orange','green')}
cluster_col <- cluster_col_func(km.out$cluster)

plot(x,col=cluster_col,cex=2,pch=1,lwd=2, 
     xlab = "Percentage of White Students", 
     ylab = "Percentage of Students Qualified for Free/Reduced Lunch",
     main = "Clustering")
points(x,col=col,pch=19)
text(.95,.95,labels = paste("Error =",toString(mean(clust_pred != df4$Title.1.Eligible))))

clust_pred_function <- function(x){ifelse(x=="2","No","Yes")}
clust_pred <- clust_pred_function(km.out$cluster)
table(clust_pred, df4$Title.1.Eligible)
mean(clust_pred != df4$Title.1.Eligible)

# SVM
y<-df4$Title.1.Eligible <- as.factor(df4$Title.1.Eligible)
x<- cbind(df4$perc_white,df4$perc_lunch)
tuned = tune.svm(y~x, data = df4, 
                 cost = 1:10, 
                 gamma = 1:10,
                 kernel = "radial",
                 tunecontrol=tune.control(cross=10))
best_gamma <- tuned$best.model$gamma
best_cost <- tuned$best.model$cost

svmfit=svm(Title.1.Eligible~perc_white+perc_lunch,data=df4,type="C",kernel="radial",cost=best_cost,gamma=best_gamma)

print(svmfit)

make.grid=function(x,n=100){
  grange=apply(x,2,range)
  x1=seq(0,1,length=n)
  x2=seq(0,1,length=n)
  expand.grid(X1=x1,X2=x2)
}

col_func <- function(x){  ifelse(x=="Yes","blue","red") }
col <-col_func(y)
xgrid=make.grid(x)
colnames(xgrid)[1] = "perc_white"
colnames(xgrid)[2] = "perc_lunch"
ygrid=predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],
     pch=20,cex=.2,
     xlab = "Percentage of White Students", 
     ylab = "Percentage of Students Qualified for Free/Reduced Lunch",
     main = "Radial Basis SVM")
points(x,col=col,pch=19)
tuned$best.performance

####################################### Insight 5 ############################################
library(maps)
library(mapdata)
library(ggmap)
set.seed(11)

# Austin
df_svm = filter(df_orig, (Title.1.Eligible == "Yes")|(Title.1.Eligible == "No")) %>%
  filter(Location.City == 'AUSTIN') %>%
  select("Longitude", "Latitude", "Title.1.Eligible")
lat <- as.numeric(df_svm$Latitude)
lon <- as.numeric(df_svm$Longitude)
T1 <- as.factor(df_svm$Title.1.Eligible)
df_austin <- data.frame(lat,lon,T1)

austin <- get_map(location= c(lon = -97.743061, lat = 30.267153), maptype = "roadmap",zoom = 11)
ggmap(austin) + 
  geom_point(data = df_austin, mapping = aes(x=lon,y=lat,colour=T1)) +
  ditch_the_axes + ggtitle("Schools of Austin")

tuned = tune.svm(T1~., data = df_austin, 
                 cost = 1:10, 
                 gamma = 1:10,
                 kernel = "radial",
                 tunecontrol=tune.control(cross=10))
best_gamma <- tuned$best.model$gamma
best_cost <- tuned$best.model$cost


svmfit=svm(T1~.,data=df_austin,type="C",kernel="radial",cost=best_cost,gamma=best_gamma)
print(svmfit)

test <- df_austin
test$pred <- predict(svmfit, df_austin)

ggmap(austin) + 
  geom_point(data = test, mapping = aes(x=lon,y=lat,colour = pred, shape = T1),size = 3) +
  ditch_the_axes +
  labs(title = "Radial Basis SVM Predictions",
      caption = paste("Misclassification error = ", toString(mean(test$pred!=test$T1))))

mean(test$pred!=test$T1)
table(test$pred,test$T1)

# SA

df_svm = filter(df_orig, (Title.1.Eligible == "Yes")|(Title.1.Eligible == "No")) %>%
  filter(Location.City == 'SAN ANTONIO') %>%
  select("Longitude", "Latitude", "Title.1.Eligible")
lat <- as.numeric(df_svm$Latitude)
lon <- as.numeric(df_svm$Longitude)
T1 <- as.factor(df_svm$Title.1.Eligible)
df_sa <- data.frame(lat,lon,T1)


sa <- get_map(location= c(lon = -98.493628, lat = 29.424122), maptype = "roadmap",zoom = 11)
ggmap(sa) + 
  geom_point(data = df_sa, mapping = aes(x=lon,y=lat,colour=T1)) +
  ditch_the_axes + ggtitle("Schools of San Antonio")

tuned = tune.svm(T1~., data = df_sa, 
                 cost = 1:10, 
                 gamma = 1:10,
                 kernel = "radial",
                 tunecontrol=tune.control(cross=10))
best_gamma <- tuned$best.model$gamma
best_cost <- tuned$best.model$cost


svmfit=svm(T1~.,data=df_sa,type="C",kernel="radial",cost=best_cost,gamma=best_gamma)
print(svmfit)

test <- df_sa
test$pred <- predict(svmfit, df_sa)

ggmap(sa) + 
  geom_point(data = test, mapping = aes(x=lon,y=lat,colour = pred, shape = T1),size = 3) +
  ditch_the_axes +
  labs(title = "Radial Basis SVM Predictions",
       caption = paste("Misclassification error = ", toString(mean(test$pred!=test$T1))))

mean(test$pred!=test$T1)
table(test$pred,test$T1)

# LA
df_svm = filter(df_orig, (Title.1.Eligible == "Yes")|(Title.1.Eligible == "No")) %>%
  filter(Location.City == "LOS ANGELES") %>%
  select("Longitude", "Latitude", "Title.1.Eligible")
lat <- as.numeric(df_svm$Latitude)
lon <- as.numeric(df_svm$Longitude)
T1 <- as.factor(df_svm$Title.1.Eligible)
df_la <- data.frame(lat,lon,T1)


la <- get_map(location= c(lon = -118.293685, lat = 34.052234), maptype = "roadmap",zoom = 11)
ggmap(la) + 
  geom_point(data = df_la, mapping = aes(x=lon,y=lat,colour=T1)) +
  ditch_the_axes + ggtitle("Schools of Los Angeles")

tuned = tune.svm(T1~., data = df_la, 
                 cost = 1:10, 
                 gamma = 1:10,
                 kernel = "radial",
                 tunecontrol=tune.control(cross=10))
best_gamma <- tuned$best.model$gamma
best_cost <- tuned$best.model$cost


svmfit=svm(T1~.,data=df_la,type="C",kernel="radial",cost=best_cost,gamma=best_gamma)
print(svmfit)

test <- df_la
test$pred <- predict(svmfit, df_la)

ggmap(la) + 
  geom_point(data = test, mapping = aes(x=lon,y=lat,colour = pred, shape = T1),size=2) +
  ditch_the_axes +
  labs(title = "Radial Basis SVM Predictions",
       caption = paste("Misclassification error = ", toString(mean(test$pred!=test$T1))))

mean(test$pred!=test$T1)
table(test$pred,test$T1)

########################################### Insight 6 ###############################################

df6 <- filter(df_orig, Title.1.Eligible == "Yes"|Title.1.Eligible == "No") %>%
     filter(Location.City=="AUSTIN") %>%
     mutate(stratio = Full.Time.Teachers / member, 
            perc_white = white / member, 
            perc_lunch = Total.Lunch / member)
x <- cbind(df6$perc_white,df6$perc_lunch)
y <- df6$Title.1.Eligible

ggplot(data = df6, mapping = aes(x=perc_white,y=perc_lunch,colour=Title.1.Eligible)) + 
  geom_point() + xlab("Percentage of White Students") + ylab("Percentage of Students Qualified for Free/Reduced Lunch") +
  ggtitle("Schools in Austin")

hc.complete=hclust(dist(x),method="complete")
plot(hc.complete)

hc.cut=cutree(hc.complete,2)
hc.cut

cluster_col_func <- function(x){ifelse(x=="1",'orange','green')}
cluster_col <- cluster_col_func(hc.cut)
col_func <- function(x){ifelse(x=="Yes","blue","red")}
col <- col_func(y)

clust_pred_function <- function(x){ifelse(x==2,"No","Yes")}
clust_pred <- clust_pred_function(hc.cut)

plot(x,col=cluster_col,cex=2,pch=1,lwd=2, 
             xlab = "Percentage of White Students", 
             ylab = "Percentage of Students Qualified for Free/Reduced Lunch",
             main = "Clustering")
points(x=x[,1],y=x[,2],col=col,pch=19)
text(.95,.95,labels = paste("Error =",toString(mean(clust_pred != df6$Title.1.Eligible))))
table(clust_pred, df6$Title.1.Eligible)