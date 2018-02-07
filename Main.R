library(readr)

setwd("C://Titanic")
train <- read_csv("Data/train.csv")
attach(train)

# Taille de notre échantillon :
n = length(PassengerId)

# Var Age
sum(is.na(Age))/n

# On trouve 20% de NA pour Age.

# => ? KNN ?

# Var Cabin
sum(is.na(Cabin))/n

# On trouve 77% de NA pour Cabin.

# On regarde seulement si on a une Cabine ou non.
for (i in 1:n) {
  if(is.na(Cabin[i])){
    train$hasCabin[i] = 0
  } else {
    train$hasCabin[i] = 1
  }
}
attach(train)

# Var Embarked
sum(is.na(Embarked))/n

# On trouve trés peu de NA (2/891).


# barplot et histogramme
barplot(table(Survived))
barplot(table(Pclass))
barplot(table(Sex))
hist(Age)
barplot(table(SibSp))
barplot(table(Parch))
hist(Fare)
barplot(table(hasCabin))
barplot(table(Embarked))

# Summary
summary(Age)
summary(Fare)
table(Survived)
table(Pclass)
table(Sex)
table(SibSp)
table(Parch)
table(hasCabin)
table(Embarked)

# Boxplot
boxplot(Age) # => Cat
boxplot(Fare) # => ???? Cat

train$catFare = rep(3,n)
for(i in 1:n) {
  if(Fare[i] <= 100){
    train$catFare[i] = 1
  } else if(Fare[i] <= 200) {
    train$catFare[i] = 2
  } 
}

train$catAge = rep(NA,n)
for(i in 1:n) {
  if (is.na(Age[i]) == FALSE){
    if(Age[i] <= 20){
      train$catAge[i] = 1
    } else if(Age[i] <= 40) {
      train$catAge[i] = 2
    } else if(Age[i] <= 60) {
      train$catAge[i] = 3
    } else {
      train$catAge[i] = 4
    }
  }
}
