########################## SET UP #############################

require(tidyverse)
require(data.world)
require(dplyr)
require(MASS)
require(ISLR)
require(tidyverse)
require(data.world)
require(ggplot2)
require(glmnet)
require(leaps)
require(boot)
#Mine
require(modelr)
require(leaps)
project<- "https://data.world/wangweiyi722/f-17-eda-project-5"

# data will take a while to read
fy13_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy13_budgeted_student_enrollment_data"),
  dataset = project
)
fy13_school_budget_data  <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy13_school_budget_data"),
  dataset = project
)
fy14_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy14_budgeted_student_enrollment_data"),
  dataset = project
)
fy15_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy15_budgeted_student_enrollment_data"),
  dataset = project
)
fy15_data_for_tableau  <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy15_data_for_tableau"),
  dataset = project
)
fy16_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy16_budgeted_student_enrollment_data"),
  dataset = project
)
fy16_school_budget_data  <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy16_school_budget_data"),
  dataset = project
)
initial_allocation_rollup_map <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_allocation_rollup_map"),
  dataset = project
)
initial_allocations_2_16_16 <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_allocations_2_16_16"),
  dataset = project
)
initial_at_risk_allocations <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_at_risk_allocations"),
  dataset = project
)
initial_budget_allocations <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_budget_allocations"),
  dataset = project
)
fy_15_budget_by_line_item <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy15_data_for_tableau"),
  dataset = project
)



############################## Insight 1 ######################################
# Add at risk budget as column
df_at_risk_am <- dplyr::filter(fy16_school_budget_data, budget_allocation_category=="At-Risk")
df_at_risk16 <- fy16_budgeted_student_enrollment_data
df_at_risk16 <- df_at_risk16[order(df_at_risk16$school_name),]
df_at_risk_am <- df_at_risk_am[order(df_at_risk_am$school_name),]
df_at_risk16$at_risk_budget <- df_at_risk_am$amount

pairs(df_at_risk16[5:12])
names(df_at_risk16)

# Remove NA
df_at_risk16 <- subset(df_at_risk16, !is.na(df_at_risk16$special_education))

equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

fit = lm(at_risk_budget~special_education, data = df_at_risk16)
summary(fit)
ggplot(data=df_at_risk16,mapping = aes(y=at_risk_budget,x=special_education)) +
  geom_point() +
  geom_smooth(method='lm', colour="red") +
  scale_y_continuous(label=scales::comma)+
  annotate("rect", xmin = 140, xmax = 300, ymin =220000, ymax =320000, fill="white", colour="red") +
  annotate("text", x = 220, y = 270000, label = equation(fit), parse = TRUE)

df_num <- df_at_risk16[c(4:12)]
regfit.full=regsubsets(at_risk_budget~.,data=df_num)
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,which.min(reg.summary$cp))


fit4 = lm(at_risk_budget~ward+special_education+homeless_foster+direct_certs, data = df_at_risk16)
summary(fit4)

ggplot(data=df_at_risk16,mapping = aes(y=at_risk_budget,x=ward+special_education+homeless_foster+direct_certs)) +
  geom_point() +
  geom_smooth(method='lm', colour="red") +
  scale_y_continuous(label=scales::comma)+
  annotate("rect", xmin = 480, xmax = 1020, ymin =220000, ymax =320000, fill="white", colour="red") +
  annotate("text", x = 750, y = 270000, label = equation(fit4), parse = TRUE)



############################## Insight 2 ######################################
# Add Total budget as column
df_total <- dplyr::filter(fy16_school_budget_data, budget_allocation_category=="Total")
dftemp <- fy16_budgeted_student_enrollment_data
dftemp <- dftemp[order(dftemp$school_name),]
df_total <- df_total[order(df_total$school_name),]
dftemp$total <- df_total$amount
df16_num <- dftemp[4:12]
summary(df16_num)

# Replace NAs with mean of column
df16_num[] <- lapply(df16_num, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Make pretty heat map
library(reshape2)
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# move vars so it's pretty
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(round(cor(df16_num),2))
# Melt the correlation matrix
melted_cormat <- melt(get_upper_tri(cormat), na.rm = TRUE)

# Create a ggheatmap

ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  # Add corrlations for graph
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Ridge Regression and Lasso
df_norm <- df16_num
df_norm[1:8] <- scale(df_norm[1:8]) # Normalize all predictors
x=model.matrix(total~.-1,data=df_norm) 
y=df16_num$total
# Ridge Regression First
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

# Fit Lasso
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
c <- coef(cv.lasso)

library("plot3D")
v1 <- df_norm$total_projected_enrollment
v2 <- df_norm$special_education
#scatter3D(v1, v2, y,theta = -40, phi = 20, pch=20,size = 5,
#          clab = "Total Budget",
#          bty="g",xlab="Projected Enrollment", ylab="# Special Education",zlab="Total Budget")



fit <- function(v){
  int = c["(Intercept)",]
  b1 = c["total_projected_enrollment",]
  b2 = c["special_education",]
  sol = int + b1*v[,1] + b2*v[,2]
  return(sol)
}

# predict values on regular xy grid
grid.lines = 30
v1.pred <- seq(min(v1), max(v1), length.out = grid.lines)
v2.pred <- seq(min(v2), max(v2), length.out = grid.lines)
#xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(fit(cbind(v1.pred,v2.pred)), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- fit(cbind(v1,v2))
par(mfrow = c(1, 3))

scatter3D(v1, v2, y,theta = 0, phi = 0, pch=20,size = 30,
          colkey = FALSE,
          bty="g",xlab="Projected Enrollment", ylab="# Special Education",zlab="Total Budget",  
          surf = list(x = v1.pred, y = v2.pred, z = z.pred,  
                      facets = NA, fit = fitpoints))
scatter3D(v1, v2, y,theta = -45, phi = 0, pch=20,size = 30,
          colkey = FALSE,
          bty="g",xlab="Projected Enrollment", ylab="# Special Education",zlab="Total Budget",  
          surf = list(x = v1.pred, y = v2.pred, z = z.pred,  
                      facets = NA, fit = fitpoints))

scatter3D(v1, v2, y,theta = 90, phi = 0, pch=20,size = 30,
          colkey = FALSE,
          bty="g",xlab="Projected Enrollment", ylab="# Special Education",zlab="Total Budget",  
          surf = list(x = v1.pred, y = v2.pred, z = z.pred,  

                                            facets = NA, fit = fitpoints))

############################## Insight 3 ######################################
df_total <- dplyr::filter(fy16_school_budget_data, budget_allocation_category=="Total")
dftemp <- fy16_budgeted_student_enrollment_data
dftemp <- dftemp[order(dftemp$school_name),]
df_total <- df_total[order(df_total$school_name),]
dftemp$total <- df_total$amount
df16_num <- dftemp[4:12]
summary(df16_num)

# Replace NAs with mean of column
df16_num[] <- lapply(df16_num, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
# Normalize all columns except total
df16_num[,-c(9)] <- data.frame(scale(df16_num[,-9])) # Normalize all predictors

library(e1071) 

#SVM
tuned = tune.svm(total~special_education, data = df16_num, 
                 cost = 1:10,
                 tunecontrol=tune.control(cross=10))
tuned
best_cost <- tuned$best.model$cost
svmfit=svm(total~special_education, data = df16_num,cost=best_cost)
#LM
fit = lm(total~special_education, data = df16_num)
#Summaries of both
summary(fit)
summary(svmfit)

library(DAAG)

par(mfrow = c(1, 1))
plot(total~special_education, data = df16_num,pch=20)
predictedY <- predict(svmfit,df16_num)
points(df16_num$special_education,predictedY,col = "red", pch=18)
abline(a=fit$coefficients[1],b=fit$coefficients[2],col="blue")

# MSE
lm.mse <-mean(fit$residuals^2)
lm.mse
error <- df16_num$total - predictedY
svm.mse <- mean(error^2)
svm.mse


sd(df16_num$total)

ggplot(data = df16_num, mapping = aes(x=total)) + 
  geom_histogram(aes(y =..density..),bins=20) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df16_num$total), sd = sd(df16_num$total)),
                colour="red") 

############################## Insight 4 ######################################
# Add At Risk Funding
df_at_risk_am <- dplyr::filter(fy16_school_budget_data, budget_allocation_category=="At-Risk")
df_at_risk16 <- fy16_budgeted_student_enrollment_data
df_at_risk16 <- df_at_risk16[order(df_at_risk16$school_name),]
df_at_risk_am <- df_at_risk_am[order(df_at_risk_am$school_name),]
df_at_risk16$at_risk_budget <- df_at_risk_am$amount
#Add Title Funding
df_at_risk_am <- dplyr::filter(fy16_school_budget_data, budget_allocation_category=="Title")
df_at_risk_am <- df_at_risk_am[order(df_at_risk_am$school_name),]
df_at_risk16$title_funding <- df_at_risk_am$amount
df_at_risk16$perc_at_risk <- df_at_risk_am$at_risk_students

#summary(df_at_risk16)
#pairs(df_at_risk16[5:14])
#names(df_at_risk16)

fit <- lm (title_funding~at_risk_budget, data = df_at_risk16)
ggplot(data=df_at_risk16, mapping = aes(x=at_risk_budget , y=title_funding,colour=school_type)) +
    geom_point() + geom_smooth(method = 'lm',colour = "red")
summary(fit)

############################## Insight 5 ######################################

# Random Forest
require(randomForest)
set.seed(101)
train=sample(1:113,80)

colnames(df_at_risk16)[10] <- paste("one_year_older")
df <- data.frame(df_at_risk16[5:14])
#summary(df)
df[] <- lapply(df, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
dim(df)
# Test for optimal mtry
oob.err=double(9)
test.err=double(9)
for(mtry in 1:9){
  fit=randomForest(at_risk_budget~.,data=df,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df[-train,])
  test.err[mtry]=with(df[-train,],mean((at_risk_budget-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))
mtry <- which.min(oob.err)

# Test for optimal number of trees
oob.err=double(6)
test.err=double(6)
ntree = seq(500,1000,100)
for(idx in 1:6){
  fit=randomForest(at_risk_budget~.,data=df,subset=train,mtry=mtry,ntree=ntree[idx])
  oob.err[idx]=fit$mse[400]
  pred=predict(fit,df[-train,])
  test.err[idx]=with(df[-train,],mean((at_risk_budget-pred)^2))
  cat(ntree[idx]," ")
}
matplot(ntree,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))
ntree <- ntree[which.min(test.err)]

rf.fit=randomForest(at_risk_budget~.,data=df,subset=train,mtry=mtry,ntree=ntree)
rf.fit

# Boosting
require(gbm)
boost=gbm(at_risk_budget~.,data=df[train,],
          distribution="gaussian",
          n.trees=ntree,shrinkage=0.01,
          interaction.depth=4)
summary(boost)
par(mfrow=c(2,2))
plot(boost,i="direct_certs",col="red")
plot(boost,i="perc_at_risk",col="blue")
plot(boost,i="title_funding",col="blue")
plot(boost,i="one_year_older",col="blue")
par(mfrow=c(1,1))

# Compare to linear
fit = lm(at_risk_budget~.,data = df[train,])
summary(fit)

# Make predictions and compare MSE
dim(df[-train,])
pred.rf = predict(rf.fit, df[-train,])
rf.err = mean((pred.rf-df[-train,])^2)/33
rf.err

pred.boost = predict(boost, df[-train,],n.trees=400)
boost.err = mean((pred.boost-df[-train,])^2)/33

pred.lm = predict(fit,df[-train,])
lm.err = mean ((pred.lm - df[-train,])^2)/33
lm.err

errors = data.frame(err = c(rf.err, boost.err,lm.err),row.names=c("Random Forest","Boosting","Least Sqaures"))
errors
# Which has the minimum error?
row.names(errors)[which.min(errors$err)]

### SVM ###
