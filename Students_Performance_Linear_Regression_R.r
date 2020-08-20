df <- read.csv('student-mat.csv', sep=';')

head(df)

summary(df)

any(is.na(df))

str(df)

df$school <- as.factor(df$school)
df$sex <- as.factor(df$sex)
df$address <- as.factor(df$address)
df$famsize <- as.factor(df$famsize)
df$Pstatus <- as.factor(df$Pstatus)
df$Mjob <- as.factor(df$Mjob)
df$Fjob <- as.factor(df$Fjob)
df$reason <- as.factor(df$reason)
df$guardian <- as.factor(df$guardian)
df$schoolsup <- as.factor(df$schoolsup)
df$famsup <- as.factor(df$famsup)
df$paid <- as.factor(df$paid)
df$activities <- as.factor(df$activities)
df$nursery <- as.factor(df$nursery)
df$higher <- as.factor(df$higher)
df$internet <- as.factor(df$internet)
df$romantic <- as.factor(df$romantic)

str(df)

library(ggplot2)
library(ggthemes)
library(dplyr)

num.cols <- sapply(df, is.numeric)

cor.data <- cor(df[,num.cols])
cor.data

library(corrgram)

library(corrplot)

corrplot(cor.data, method='color')

corrgram(df)

corrgram(df, order=T, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

ggplot(df, aes(x=G3)) + geom_histogram(bins=20,alpha=0.5, fill='blue')

library(caTools)

set.seed(101)

sample <- sample.split(df$G3, SplitRatio=0.7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

model <- lm(G3 ~. , train)

summary(model)

res <- residuals(model)
class(res)

res <- as.data.frame(res)
head(res)

ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5)

plot(model)

G3.predictions <- predict(model,test)

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
head(results)

to_zero <- function(x){
    if(x<0){
        return(0)
    }else{
        return(round(x,0))
    }
} 

results$predicted <- sapply(results$predicted,to_zero)

head(results)

mse <- mean((results$actual - results$predicted)^2)
print('MSE')
print(mse)

print('RMSE')
print(mse^0.5)

SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$G3) - results$actual)^2)
R2 <- 1 - SSE/SST
print('R2')
print(R2)

write.csv(results, 'predictions.csv')


