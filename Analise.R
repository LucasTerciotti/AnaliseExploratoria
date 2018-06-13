#PREPARAÇÃO DOS DADOS - Feita posteriormente

titanic <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/titanic.csv"),
                    stringsAsFactors=FALSE,
                    na.strings = c(""))
library(tidyverse)
library(ggplot2)

str(titanic)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Survived <- as.factor(titanic$Survived)
summary(titanic$Embarked)
titanic$Embarked[is.na(titanic$Embarked)] = "S"
titanic$Embarked <- factor(titanic$Embarked)
summary(titanic$Embarked)

str(titanic)

#SEXO:
titanic$Sex <- as.factor(titanic$Sex)
summary(titanic$Sex)
contrasts(titanic$Sex)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp +
                  Parch + Fare + Embarked,
                data=titanic[!is.na(titanic$Age),], 
                method="anova")

titanic$Age[is.na(titanic$Age)] <-
  
  predict(Agefit, titanic[is.na(titanic$Age),])

summary(titanic$Age)

#Análise Exploratória
#testes:
glimpse(titanic)
filter(titanic, Survived == 1, Age == 30) #permite filtrar os dados para alcançar informações específicas
select(titanic,Survived, Age, Sex, PassengerId)
summarise(titanic, Age = mean(titanic$Age))

#Curiosidade: Qual a média de idade dos que morreram? e dos que sobreviveram?
Alive <- group_by(titanic, Survived)
summarise(Alive, Age = mean(Age, na.rm = TRUE))

#Métricas básicas: Six number summary
summary(titanic)

#Boxplot:
ggplot(data = titanic, aes(x = "", y = Age) ) + 
  geom_boxplot() +
  geom_jitter()

ggplot(data = titanic, aes(x=Sex, y=Age)) + 
  geom_boxplot(aes(color=Survived))

ggplot(data = titanic, aes(x=Survived, y=Age)) + 
  geom_boxplot(aes(color= Embarked))

#scatterplot (Age ~Survived)
ggplot(data = titanic) + 
  geom_point(mapping = aes(x = Age, y = Survived))

#scatterplot (Age ~Embarked)
ggplot(data = titanic) + 
  geom_point(mapping = aes(x = Age, y = Embarked))

ggplot(data = titanic) + 
  geom_point(mapping = aes(x = Age, y = Embarked, color = Survived))

#scatterplot (Sex ~ Age)
ggplot(data = titanic) + 
  geom_point(mapping = aes(x = Sex, y = Age))

ggplot(data = titanic) + 
  geom_point(mapping = aes(x = Sex, y = Age, color = Survived))

ggplot(data = titanic) + 
  geom_point(mapping = aes(x = PassengerId, y = Sex, color = Survived)) + 
  facet_wrap( ~ Age )

#Density
ggplot(data = titanic, aes(Age)) + 
  geom_density(aes(fill=Survived,color=Survived), alpha=0.4)

#histogram
ggplot(data = titanic, aes(Age)) + 
  geom_histogram(aes(color=Survived, fill=Survived), alpha=0.5)




