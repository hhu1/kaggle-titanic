setwd("~/Documents/kaggle/titanic")


# visualization by Pclass and levels
train = read.csv("train.csv")
train$Survived <- factor(train$Survived, levels=c(0,1))

test = read.csv("test.csv")


library("ggplot2")
ggplot(train, aes(Survived)) + geom_histogram(aes(fill=Survived)) +facet_grid(Sex~.)
ggplot(train, aes(Survived)) + geom_histogram(aes(fill=Survived)) +facet_grid(Pclass~.)


# combine train and testing
test$Survived = NA
all = rbind(train, test)
all = all[, -c(4,9,11)]
all$Pclass = factor(all$Pclass)

# use MICE to impute missing data
outcome = all$Survived
all = complete(mice(all),action=1)
all$Survived=outcome

train = subset(all, !is.na(Survived))
test = subset(all, is.na(Survived))

# run random forest
rf  = randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train)
pred = predict(rf, test)
write.table(file="rf-sub.csv", x=data.frame(PassengerId=test$PassengerId, Survived=pred), quote=F, sep=',', row.names=F)
