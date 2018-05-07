require(MASS)
require(ISLR)
require(tidyverse)
require(dplyr)
require(data.world)
require(ggplot2)

################ Reading the data from data.world #############################
project<- "https://data.world/isabelcachola/f-17-eda-project-3"
data.world::set_config(cfg_env("DW_API"))

df_w <- data.world::query(
  data.world::qry_sql("SELECT * FROM kickstarter_data_with_features_2"),
  dataset = project
)


################ Insight 1 #####################################
sdf_w = dplyr::select(df_w,country, created_at,backers_count,state)
sdf_w = dplyr::filter(sdf_w,country=="US",state %in% c("failed","successful"),backers_count<50000)

backersplot <- ggplot(sdf_w)+geom_point(aes(x=created_at,y=backers_count,color=state))
plot(backersplot)

## LOOCV Leave One Out Cross Validation
glm.fit=glm(backers_count~created_at, data=sdf_w) # Create a linear model
summary(glm.fit)

##Function using the formula for loocv for linear
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

# fit some polynomials degrees 1-5
cv.error=rep(0,10)
degree=1:10
for(d in degree){
  glm.fit=glm(backers_count~poly(created_at,d), data=sdf_w) #make a model for each degree
  cv.error[d]=loocv(glm.fit) #Computes LOOCV error and puts it into error vector
}
summary(glm.fit)
plot(degree,cv.error,type="b") 
## 10-fold CV
# divide data into 10 pieces. Use 9 for training 1 for testing. Then proceed same as LOOCV
cv.error10=rep(0,10)
for(d in degree){
  glm.fit=glm(backers_count~poly(created_at,d), data=sdf_w)
  cv.error10[d]=cv.glm(sdf_w,glm.fit,K=10)$delta[1]
}
summary(glm.fit)
lines(degree,cv.error10,type="b",col="red")


new.glm.fit = glm(backers_count~poly(created_at,5),data=sdf_w)
summary(new.glm.fit)

####################### Insight 2 ########################################
require(leaps)
df_w2 <- data.world::query(
  data.world::qry_sql("SELECT * FROM Joined_Census"),
  dataset = project
)
pairs(df_w2[,-1])

regfit.full_w=regsubsets(percent_success~.,data=df_w2[,c(-1,-2,-3)],nvmax=5)
reg_w_summary=summary(regfit.full_w)
plot(reg_w_summary$cp,xlab="Number of Variables",ylab="Cp")

###################### Insight 3 ###########################################
require(glmnet)
# Perform Ridge Regression
df_w3 <- data.world::query(
  data.world::qry_sql("SELECT * FROM Individual_KS_with_Census_Data"),
  dataset = project
)
x=model.matrix(status~.-1,data = df_w3)
x=x[,c("percent_divorced_male","percent_tech_business","population_age_30to59","percent_college_degree","percent_above_150k_income")]
y=df_w3$status
y=as.integer(as.logical(y))
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
# Perform Lasso Regression
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE) 
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso) 

#Use LDA to make predictions using the model suggested by the regression.
#Set training and testing data
set.seed(1)
train_ind <- sample(seq_len(nrow(df_w3)), size = 10000)

training <- df_w3[train_ind, ]
testing <- df_w3[-train_ind, ]

# Use the perc_minority column from the testing data to make a lda model predicting whether Trump or Clinton won that state
divorced_lda = lda(status~percent_divorced_male,data = training)
divorced_lda
divorced_lda_pred = predict(divorced_lda,testing)
table(divorced_lda_pred$class,testing$status)

college_lda = lda(status~percent_college_degree,data = training)
college_lda
college_lda_pred = predict(college_lda,testing)
table(college_lda_pred$class,testing$status)

tech_business_lda = lda(status~percent_tech_business,data = training)
tech_business_lda
tech_business_pred = predict(tech_business_lda,testing)
table(tech_business_pred$class,testing$status)

################## Insight 4 ################################################
count_lm = lm(percent_success~ct_by_state,data=df_w2)
count_lm
summary(count_lm)
count_lm_2 = lm(percent_success~poly(ct_by_state,2),data=df_w2)
count_lm_2
summary(count_lm_2)
newdat = data.frame(ct_by_state = seq(min(df_w2$ct_by_state), max(df_w2$ct_by_state), length.out = 100))
newdat$pred = predict(count_lm_2, newdata = newdat)
plot(percent_success~ct_by_state,data = df_w2)
with(newdat,lines(x=ct_by_state,y=pred))

# fit some polynomials degrees 1-5
cv.error_count=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit_count=glm(percent_success~poly(ct_by_state,d), data=df_w2) #make a model for each degree
  cv.error_count[d]=loocv(glm.fit_count) #Computes LOOCV error and puts it into error vector
}
plot(degree,cv.error_count,type="b") 
## 10-fold CV
# divide data into 10 pieces. Use 9 for training 1 for testing. Then proceed same as LOOCV
cv.error10_count=rep(0,5)
for(d in degree){
  glm.fit_count=glm(percent_success~poly(ct_by_state,d), data=df_w2)
  cv.error10_count[d]=cv.glm(df_w2,glm.fit_count,K=10)$delta[1]
}

lines(degree,cv.error10_count,type="b",col="red")
cv.error_count
cv.error10_count

new.glm.fit_count = glm(percent_success~poly(ct_by_state,1),data=df_w2)
summary(new.glm.fit_count)

# Take out California and rerun the models and cross validation
new_df_w2 = df_w2
new_df_w2 = new_df_w2[new_df_w2[,1]!="CA",]

count_lm_1 = lm(percent_success~poly(ct_by_state,1),data=new_df_w2)
count_lm_1
summary(count_lm_1)
count_cases = ggplot(predict_success,aes(x=perc_succ,y=count_lm_1))
count_cases
newdat = data.frame(ct_by_state = seq(min(new_df_w2$ct_by_state), max(new_df_w2$ct_by_state), length.out = 100))
newdat$pred = predict(count_lm_1, newdata = newdat)
plot(percent_success~ct_by_state,data = new_df_w2)
with(newdat,lines(x=ct_by_state,y=pred))

# fit some polynomials degrees 1-5
cv.error_count=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit_count=glm(percent_success~poly(ct_by_state,d), data=new_df_w2) #make a model for each degree
  cv.error_count[d]=loocv(glm.fit_count) #Computes LOOCV error and puts it into error vector
}
plot(degree,cv.error_count,type="b") 
## 10-fold CV
# divide data into 10 pieces. Use 9 for training 1 for testing. Then proceed same as LOOCV
cv.error10_count=rep(0,5)
for(d in degree){
  glm.fit_count=glm(percent_success~poly(ct_by_state,d), data=new_df_w2)
  cv.error10_count[d]=cv.glm(new_df_w2,glm.fit_count,K=10)$delta[1]
}

lines(degree,cv.error10_count,type="b",col="red")
cv.error_count
cv.error10_count

new.glm.fit_count = glm(percent_success~poly(ct_by_state,1),data=df_w2)
summary(new.glm.fit_count)
