#Importing libraries
library(ggplot2)

#Importing data
data <- read.csv("goldnew.csv")


#Summary of data
summary(data)


#Visualizing relationship between independent and dependent variables
ggplot()+
  geom_line(aes(x = data$Usd_Inr,
                y = data$Gold_Price),
            colour = 'green') +
  ggtitle("Gold Price vs USD INR exchange rate") +
  labs(x = "USD price in INR",
       y = "Gold Price in INR")


ggplot()+
  geom_line(aes(x = data$Crude_Oil,
                y = data$Gold_Price),
            colour = 'blue') +
  ggtitle("Gold Price vs Crude oil") +
  labs(x = "Crude Oil price in INR per barrel",
       y = "Gold Price in INR")


ggplot(data, aes(y = Gold_Price, colour = ''))+
  geom_line(aes(x = Sensex,
                colour = 'red')) +
  ggtitle("Gold Price vs Sensex score") +
  labs(x = "Sensex score",
       y = "Gold Price in INR")

#Splitting data into test and training set
split <- floor(nrow(data)*0.8)
training_set <- data[1:split,]
test_set <- data[split:nrow(data),]


#Training the model
model <- lm(Gold_Price ~ Usd_Inr + Sensex + Crude_Oil , data = training_set)


#Predicting with test set and model
predictedValues <- predict(model, test_set)
print(predictedValues)  

#Summary of predicted values
summary(predictedValues)


#Summary of actual close
summary(test_set$Gold_Price)

#Storing in result dataset and getting column for difference in predicted and actual value
result_set <- data.frame(test_set, 
                         predictedValues, 
                         test_set$Gold_Price - predictedValues)

#Changing column name to Difference
colnames(result_set)[8] <- "Difference"

#plotting predicted vs real values
ggplot(result_set,aes(x = Usd_Inr, colour = '')) +
  geom_line(aes(y = predictedValues,
                colour = 'predicted values')) +
  geom_line(aes(y = Gold_Price,
                colour = 'real values')) +
  ggtitle("Predicted values and real values") +
  labs(x = 'USD price in INR',
       y = 'Gold price in INR')

#model accuracy
summary(model)


#viewing resultant dataset
View(result_set)

#RMSE
diffSquare <- result_set$Difference^2
rmse <- sqrt(mean(diffSquare))
print(paste("Root mean square error value: ",rmse))