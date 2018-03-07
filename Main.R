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
# => on remplace NA par S
train$Embarked[is.na(train$Embarked)]='S'

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

myMax<-function(x){
  max = -1
  for (i in 1:length(x)){
    if (x[i] > max){
      max = x[i]
      index = i-1
    }
  }
  return(index)
}

err<-function(dataFrames,Ystring,col){
  d = NULL
  list = divVal(dataFrames,col)
  vals = names(list)
  n = length(dataFrames[Ystring])
  for(i in vals){
    divCol = divVal(dataFrames,col)
    d[i] = myMax(table(divCol[[i]][Ystring]))
  }
  return(d)
}

unionData <-function(list,d){
  data1=NULL
  data2=NULL
  vals = names(list)
  for (i in vals){
    if(d[i] == 0){
      data1 = rbind(data1,list[[i]])
    }else if(d[i] == 1){
      data2 = rbind(data2,list[[i]])
    }
  }
  return(list(data1,data2))
}

gini<-function(dataFrames,Ystring){
  cols <- names(dataFrames)[names(dataFrames) != Ystring]
  G = list()
  p = c()
  for (i in cols) {
    vals = c(unique(dataFrames[i]))
    print(vals)
    col <- eval(parse(text=i))
    d = err(dataFrames,Ystring,col)
    list = unionData(divVal(dataFrames,col),d)
    for(j in 1:2){
      m = length(t(list[[j]][Ystring]))
      card = table(list[[j]][Ystring])
      p=(1/m)*card
      G[[i]][j] = 1 - sum(p^2)
    }
  }
  return(G)
}

G = gini(train1,'Survived')

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

d = err(train1,'Survived',Pclass)
pclassDiv = divVal(train1,Pclass)
union = unionData(pclassDiv,d)
