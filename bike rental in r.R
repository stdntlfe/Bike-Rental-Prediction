
set.seed(123)
#import dataset
data=read.csv(file.choose(),sep = ',',header = T)
# View dataset
View(data)
#feature selection
data=data[,3:16]
# After feature selection view the dataset
View(data)
# Understand the dataset 
str(data)
#Attach the datset with the system so we can use directly variable
attach(data)
#View the dataset on console
head(data)
# Normalization of the few columns  like casual and registered
data$casual=(data$casual-min(data$casual))/(max(data$casual)-min(data$casual))
data$registered=(data$registered-min(data$registered))/(max(data$registered)-min(data$registered))
head(data)
# Devide dataset in the train and test case
ind= sample(2,nrow(data),replace=T,prob=c(0.8 ,0.2))
train=data[ind==1,]
test=data[ind==2,]
# Apply linear regresson on the dataset
model=lm(cnt~.,train)
#find rmse value 
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals 
predictionRMSE <- rmse(error) 
predictionRMSE
# Predict the dataset
predictedy=predict(model,test)
predictedy
#plot the model
plot(model)
#View the whole summary of the model
summary.lm(model)

