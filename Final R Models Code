rm(list=ls())
library(maps)
library(tree)
library(randomForest)
library(tidyverse)

library(ggplot2)
library(reshape2)
# install.packages("readxl")

library('lubridate')#

library(geosphere)
library(dplyr)

library(plotly)




set.seed(5)         

setwd('D:/stat_wk')
dff = read.csv("taxi_fares.csv")
dff_org= read.csv("taxi_fares.csv")
# dff=read.table("taxi_fares.xlsx",header=TRUE)


#EDA

dff$pickup_datetime=as_datetime(dff$pickup_datetime)
dff_org$pickup_datetime=as_datetime(dff_org$pickup_datetime)

summary(dff)



#Observing location, time and count based plots
plot(dff$pickup_longitude,dff$fare_amount,col=as.factor(dff$passenger_count))
plot(dff$pickup_latitude,dff$fare_amount,col=as.factor(dff$passenger_count))

plot(dff$pickup_datetime,dff$fare_amount,col=as.factor(dff$passenger_count))


par(mfrow=c(1,3))
boxplot(dff$pickup_datetime,xlab='datetime')
boxplot(dff$pickup_longitude,xlab='pickup_long')
boxplot(dff$pickup_latitude,xlab='pickup_lat')



#BASELINE MODEL
summary(lm(fare_amount~.,dff))

n_row=nrow(dff)
test=sample(n_row,200)

temp_mlr=lm(fare_amount~.,dff[-test,])
temp_pre=predict(temp_mlr,dff[test,])
cat(mean((dff[test,c('fare_amount')]-temp_pre)^2))




#The adjusted R square of the model based on the raw data was quite low, we realized that the model is not able to explain the variable properly
#The error was too high 5.9 for a mean score of fare_amount of ~9.8

#We had datetime and location centric data so we decided to start out with creating distance and time based basic features
#Linear to non linear parameters (combination of predictors)

##Feature Creation

dff$day_of_the_week=as.factor(weekdays(as.Date(dff$pickup_datetime)))
dff$weekend=ifelse(dff$day_of_the_week %in% c('Saturday','Sunday'),1,0)
dff=dff[,-8]# - ('day_of_the_week')]
dff$hour_of_the_day=as.integer(hour(dff$pickup_datetime))



dff$dist_km=distHaversine(cbind(dff$pickup_longitude,dff$pickup_latitude ), cbind(dff$dropoff_longitude,dff$dropoff_latitude ))/1000


##Exploratory DATA Analysis
summary(dff)
#Zoomed in quantile values
# as.data.frame(quantile(df$dare_amount,probs=c(0.25,0.5,seq(0.75,1,0.025))))
var(dff$pickup_latitude)# Quite low 
var(dff$dist_km)#Appreciable spreaded over wide range of values, can help to build better predictive models

hist(dff$dist_km)
hist(log(dff$dist_km))#Seems to be more normaly distributed can be equipped in our model

plot(dff$dist_km,dff$fare_amount,ylab='fare_amount')

#Unique instances of each column
n_u=c()
v=c()
m=c()
for (i in names(dff)){
  n_u=c(n_u,length(unique(dff[,i])))
  v=c(v,sqrt(var(dff[,i])))
  m=c(m,mean(dff[,i]))
}

print(data.frame(col_names=names(dff),n_unique=n_u,std_dev=v,mean=m))

#plotted combinations of pairplots to obtain a comprehensive view
pairs(dff[-c(1)])





num_dff=dff[-c(1)]

## Plot against gRatings
par(mfrow=c(3,3))
for (i in names(num_dff)){
  if (i != "fare_amount"){
    plot(num_dff$fare_amount,num_dff[,i],main=paste('col=',i),xlab='fareAmount',ylab=i)
  }}


#Generated a heatmap based correlation plot between columns to get an idea of which ones are associated with one another and to what extent
heatmap(cor(num_dff,method='pearson'),)
#Trying to analyse which pair of columns has high degree of correlation in absolute terms
heatmap((abs((cor(num_dff,method='pearson')))>0.6)*1)

df_cor=cor(num_dff,method='pearson')


melted_cormat = melt(df_cor)
head(melted_cormat)




# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) 


ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
 )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



### Individual predictor analysis with greviews
coeff_pre=c()
pval_pre=c()

num_dff=dff

for (pre in names(num_dff)){
  if (pre =='fare_amount')
  {next}
  cat(pre,'\n')
  
  temp_pre=num_dff[,pre]
  model_pre=lm(fare_amount~temp_pre,data=num_dff)
  
  sum_pre=summary(model_pre)
  
  coeff_pre=c(coeff_pre,model_pre$coefficients['temp_pre'])
  pval_pre=c(pval_pre,sum_pre$coefficients[2,4])
  
}

df_pre=data.frame(predictors=names(num_dff)[-2],coeff=coeff_pre,p_vals=pval_pre)

#sorting wrt absolute values of coefficients
df_pre=df_pre[order(abs(df_pre$coeff),decreasing=TRUE),]


dev.new(width=5, height=4)
plot(dff$fare_amount,dff$hour_of_the_day)
dev.off()

#MLR
model_all=lm(fare_amount~.,data=num_dff)

summary(model_all)
model_pre_all=coefficients(model_all)



###Combinations of 3 predictors 
n_row=nrow(num_dff)
test=sample(n_row,200)
names(num_dff)

pre_lt=names(num_dff)[-2]
temp=combn(1:length(pre_lt),3,simplify=FALSE)


test_mse=c()
names_col=c()
for (i in temp){
  # print(num_dff[-test,c(i)])
  
  names_col=rbind(names_col,pre_lt[i])
  cat(pre_lt[c(i)],'\n')
  
  temp_df=data.frame(num_dff[,c(pre_lt[c(i)],'fare_amount')])
  train_df=temp_df[-test,]
  test_df=temp_df[test,]
  
  temp_model=lm(fare_amount~ .,data=train_df)
  pred=predict(temp_model,test_df)
  
  err=mean((num_dff[test,c('fare_amount')]-pred)^2)
  test_mse=c(test_mse,err)
  
  
}

err_df_3=data.frame(col1=names_col[,1],col2=names_col[,2],col3=names_col[,3],err=test_mse)
err_df_3=err_df_3[order(err_df_3$err),]

temp_mlr=lm(fare_amount~.,dff_org[-test,])
temp_pre=predict(temp_mlr,dff_org[test,])
cat(mean((dff_org[test,c('fare_amount')]-temp_pre)^2))

####
temp_df=num_dff
train_df=temp_df[-test,]
test_df=temp_df[test,]

temp_model=lm(fare_amount~ .,data=train_df)
pred=predict(temp_model,test_df)

err=mean((num_dff[test,c('fare_amount')]-pred)^2)



priority_ord=c('hour_of_the_day','passenger_count','dist_km')
# priority_ord=c('dist_km','hour_of_the_day','passenger_count')


tbu=c()

err=matrix(-1,1000,3)

r=0
for (i in priority_ord){
  tbu=c(tbu,i)
  temp=dff[,c(tbu,'fare_amount')]
  
  
  cat(names(temp),'\n')
  r=r+1
  for (j in 1:1000){
    
    # test=sample(nrow(dff),order((nrow(dff)/5),0))
    test=sample(1:1000,100)
    
    df_train=temp[-test,]
    df_test=temp[test,]
    
    mod=lm(fare_amount~.,df_train)
    
    pre=predict(mod,df_test)
    
    err[j,r]=mean((df_test[,'fare_amount']-pre)^2)
  } 
}




# dff_err=data.frame(err)
# names(dff_err)=c('Dist','Dist_passenger','Dist_passenger_hour')
# par(mfrow=c(1,3))
# boxplot(dff_err$Dist,xlab='pred-Dist')
# boxplot(dff_err$Dist_passenger,xlab='pred-Dist-PassengerNumber')
# boxplot(dff_err$Dist_passenger_hour,xlab='pred-Dist-PassengerNumber-Hour')


dff_err=data.frame(err)
names(dff_err)=c('hour','hour_passenger','hour_passenger_dist')
par(mfrow=c(1,3))
boxplot(dff_err$hour,xlab='pred-Hour')
boxplot(dff_err$hour_passenger,xlab='pred-Hour-PassengerNumber')
boxplot(dff_err$hour_passenger_dist,xlab='pred-Hour-PassengerNumber-Dist')


summary(dff_err)


#Subset Selection
#install.packages('leaps')
library(leaps)


model_sub=regsubsets(fare_amount~.,data=num_dff,nvmax=9)
s=summary(model_sub)


par(mfrow=c(2,2))
plot(s$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
plot(s$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")



head(dff)
write_csv(dff,'processed_taxi_fare.csv')
plot(dff$dist_km,dff$fare_amount,col=as.factor(dff$passenger_count))
plot(dff$hour_of_the_day,dff$fare_amount,col=as.factor(dff$passenger_count))


# plot_ly(dff,x=~dist_km,z=~fare_amount,y=~passenger_count)

