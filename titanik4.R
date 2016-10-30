library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr')
library('mice') 
library('randomForest') 
library('rpart')
library('rpart.plot')
library('RColorBrewer')

# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv("/home/alyona/R/train.csv")

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv("/home/alyona/R/test.csv")

# Print the structure of data frames
str(test)
str(train)

train$Name <- as.character(train$Name)
test$Name <- as.character(test$Name)

train <- as_data_frame(train)
test <- as_data_frame(test)

full_data <- as_data_frame(bind_rows(train, test))

# Get all titles from passenger names
full_data$Title <- sapply(full_data$Name, function(x) 
                          {strsplit(x, split='[,.]')[[1]][2]})
full_data$Title <- sub(' ', '', full_data$Title)

# Show title counts by sex
table(full_data$Sex, full_data$Title)

# Rename some titles  
full_data$Title[full_data$Title %in% c('Mlle', 'Miss', 'Ms')] <- 'Miss'
full_data$Title[full_data$Title == 'Mme'] <- 'Mrs' 
full_data$Title[full_data$Title %in% c('Dona', 'Lady', 'the Countess')]  <- 'Lady'
full_data$Title[full_data$Title %in% c('Don', 'Capt', 'Col', 'Jonkheer', 'Major', 'Sir')]  <- 'Sir'

# Show title counts by sex
table(full_data$Sex, full_data$Title)

full_data$Title <- as.factor(full_data$Title)

# Print the structure of data frame
str(full_data)

# Get surnames from passenger names
full_data$Surname <- sapply(full_data$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})

# Create a variable "family size" including the passenger themselves
full_data$Family <- full_data$SibSp + full_data$Parch + 1

full_data$Family[full_data$Family %in% c(2:4)] <- '2-4'
full_data$Family[full_data$Family > 4] <- '5+'
full_data$Family <- as.factor(full_data$Family)

str(full_data)

full_data$Deck <- as.factor(sapply(full_data$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

embark <- full_data[full_data$Embarked == '',]

tree_1 <- rpart(Embarked ~ Pclass + Fare, data = full_data, method = "class")
plot(tree_1)
text(tree_1)

prediction_embark <- predict(tree_1, embark, type = "class")

full_data[c(62, 830),]$Embarked <- 'S'

f <-full_data[is.na(full_data$Fare),]

aggregate(Fare ~ Embarked + Pclass, full_data, median)

full_data[is.na(full_data$Fare),]$Fare <- 8.05

full_data[1044,]

tree_2 <- rpart(Age ~ Sex + Fare + Title + SibSp + Parch, data = full_data[!is.na(full_data$Age),], 
                method = "anova")
plot(tree_2)
text(tree_2)

full_data[is.na(full_data$Age),]$Age <- predict(tree_2, full_data[is.na(full_data$Age),])

full_data$Age <- round(full_data$Age, 2)

train_1 <- full_data[1:891,]
test_1 <- full_data[892:nrow(full_data),]

train_1$Embarked <- as.factor(train_1$Embarked)

#tree <- rpart(Survived ~ Sex + Title + Pclass + Age + Fare + Parch + SibSp + Family + Deck, data = train_1,
 #             method = "class")
#fancyRpartPlot(tree)

#plot(tree)
#text(tree)

#prediction <- predict(tree, test_1, type="class")
#solution <- data.frame(PassengerId = test_1$PassengerId, Survived = prediction)
#write.csv(solution, file = "solution.csv", row.names = F)

set.seed(222)

forest <- randomForest(as.factor(Survived) ~ Sex + Title + Pclass + Age + Fare + SibSp + Parch, 
                       train_1, importance = T, ntree = 2000)

prediction_1 <- predict(forest, test_1)
solution_1 <- data.frame(PassengerId = test_1$PassengerId, Survived = prediction_1)
write.csv(solution, file = "solution_1.csv", row.names = F)

varImpPlot(forest)

tree_3 <- rpart(Survived ~ Sex + Title + Pclass + Age + Fare + SibSp + Parch, data = train_1, 
                method = "class", control = rpart.control(minsplit = 50, cp = 0))

plot(tree_3)
text(tree_3)

prediction_2 <- predict(tree_3, test_1, type="class")
solution_2 <- data.frame(PassengerId = test_1$PassengerId, Survived = prediction)
write.csv(solution, file = "solution_2.csv", row.names = F) 
