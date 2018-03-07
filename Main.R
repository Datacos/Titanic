# TER Titanic

# Rq : Vérifier si val mq dans data de validation

library(rpart)
library(readr)

setwd("./TER/trunk/Data")
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
train$hasCabin = train$Cabin
train$hasCabin[!is.na(train$hasCabin)] = 1
train$hasCabin[is.na(train$hasCabin)] = 0

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

train$catFare = train$Fare
train$catFare[train$catFare <= 100 ] = 1
train$catFare[train$catFare <= 200 & train$catFare > 100 ] = 2
train$catFare[train$catFare > 200 ] = 3

train$catAge = train$Age
train$catAge[is.na(train$catAge)] = 0
train$catAge[train$catAge > 0 & train$catAge <= 20] = 1
train$catAge[train$catAge > 20 & train$catAge <= 40] = 2
train$catAge[train$catAge > 40 & train$catAge <= 60] = 3
train$catAge[train$catAge > 60] = 4

attach(train)

plot(Age,Fare)

fit <- rpart(Survived ~ hasCabin + catAge + catFare + Sex + Parch +Pclass + SibSp +Embarked, data = train, method = 'class',control = rpart.control(minsplit = 30, cp=0.005))
plot(fit)
text(fit)

train1 = subset(train,select = -c(PassengerId,Age,Ticket,Fare,Cabin,Name))

div<-function(dataFrame,X,x){
  return(subset(dataFrame,X == x))
}

divVal<-function(dataFrames,val){
  list1 = list()
  for (i in unique(val)){
    div1 = div(dataFrames,val,i)  
    list1 = c(list1,list(div1))
  }
  names(list1) <- unique(val)
  return(list1)
}

tri<-function(listdata,Ystring){
  listLen = length(listdata)
  for (i in 1:listLen){
    print(table(listdata[[i]][Ystring]))
  }
}

gini<-function(dataFrames,Ystring){
  cols <- names(dataFrames)[names(dataFrames) != Ystring]
  G = list()
  p = c()
  for (i in cols) {
    vals = c(unique(dataFrames[i]))
    col <- eval(parse(text=i))
    list = divVal(dataFrames,col)
    for(j in unique(dataFrames[i])){
      print(j)
      m = length(t(list[[j]][Ystring]))
      print(m)
      card = table(list[[j]][Ystring])
      p=(1/m)*card
      print(p)
      G[[i]][j] = 1 - sum(p^2)
    }
  }
  return(G)
}

gini(train1,'Survived')

table(div(train,hasCabin,0)$Survived)

arbre<-function(dataFrames,Y,node = 1){
  datalen <- length(dataFrames)
  valString <- names(dataFrames)[datalen]
  val <- eval(parse(text=valString))
  Ystring <- deparse(substitute(Y))
  if (datalen == 1){
    print('Hello world')
  }
  listdata <- divVal(dataFrames,val)
  tri(listdata,Ystring)
  
}
arbre(train1,Survived)

for (i in c('foo','bar')){
  print(i)
}

