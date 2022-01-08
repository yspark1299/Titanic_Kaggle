train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

str(train)
str(test)

summary(train)
summary(test)
Survival <- ifelse(train$Survived==1, "Survived","Dead")



library("ggplot2")
ggplot(train,aes(x=Sex, fill=Survival)) + facet_wrap(~Pclass) + geom_bar(width=0.5) + ggtitle("Survival rate based on Sex and Pclass")
ggplot(train,aes(y=Age, x=Survival,color=Sex)) + geom_violin() + ggtitle("Survival rate based on Age and Sex")
ggplot(train,aes(x=Age,fill=Survival)) + geom_histogram(binwidth=5,color="black") + ggtitle("Survival rate based on Age")

mosaicplot(table(ifelse(train$Survived==1,"Survived","Dead"),train$Sex),color=TRUE,main="Survival rate based on Sex") 
mosaicplot(table(ifelse(train$Survived==1,"Survived","Dead"),train$Pcla),color=TRUE,main="Survival rate based on Pclass")


train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE
test$Survived <- NA

combi_titanic <- rbind(train, test)
table(combi_titanic$IsTrainSet)

colSums(is.na(combi_titanic)|combi_titanic=='')



head(combi_titanic$Name)



combi_titanic$Title <- gsub("^.*, (.*?)\\..*$", "\\1", combi_titanic$Name)
table(combi_titanic$Sex, combi_titanic$Title)
combi_titanic$Title[combi_titanic$Title == 'Mlle' | combi_titanic$Title=='Ms'| combi_titanic$Title == 'Lady'] <- 'Miss'
combi_titanic$Title[combi_titanic$Title == 'Major'| combi_titanic$Title == 'Sir'
                    |combi_titanic$Title == 'Master'|combi_titanic$Title == 'Rev'] <- 'Mr'
combi_titanic$Title[combi_titanic$Title == 'Mme'] <- 'Mrs'
combi_titanic$Title[combi_titanic$Title == 'Capt' |combi_titanic$Title == 'Col' |combi_titanic$Title == 'Don'
                    |combi_titanic$Title == 'Dona'|combi_titanic$Title == 'Dr' |combi_titanic$Title == 'Jonkheer'
                    | combi_titanic$Title =='the Countess'] <- 'Other'

mosaicplot(table(ifelse(train$Survived==1,"Survived","Dead"),train$Title),color=TRUE,main="Survival rate based on Title") 


combi_titanic$IsMale <- ifelse(combi_titanic$Sex=="male",1,0) # indicator for ML 
#male=1 female =0
combi_titanic$Sex <- NULL

combi_titanic$Embarked[combi_titanic$Embarked == 'C']<-'C'
combi_titanic$Embarked[combi_titanic$Embarked == 'Q']<-'Q'
combi_titanic$Embarked[combi_titanic$Embarked == 'S']<-'S'
mosaicplot(table(ifelse(train$Survived==1,"Survived","Dead"),train$Embarked),color=TRUE,main="Survival rate based on Embarked") 

                    
combi_titanic$Pclass <- as.factor(combi_titanic$Pclass)
combi_titanic$Embarked <- as.factor(combi_titanic$Embarked)
combi_titanic$Age <- as.factor(combi_titanic$Age)
combi_titanic$Fare <- as.factor(combi_titanic$Fare)
combi_titanic$Title <- as.factor(combi_titanic$Title)
combi_titanic$IsMale <- as.factor(combi_titanic$IsMale)





train <- combi_titanic[combi_titanic$IsTrainSet==TRUE,]
test <- combi_titanic[combi_titanic$IsTrainSet==FALSE,]

train$Survived <- as.factor(train$Survived)

library(e1071)

model <- naiveBayes(Survived~., train)
str(model)
prediction <- predict(model, test)
str(prediction)
summary(prediction)
output <- data.frame(test$PassengerId, prediction)
str(output)

Sex <- ifelse(test$IsMale==1,"Male","Female")
Survived_s <- ifelse(output$prediction ==1,"Survived","Dead")
table(Sex, Survived_s)

ggplot(test,aes(y=Age, x=Sex,color=Sex)) + geom_violin() + ggtitle("Bording rate on Age and Sex(test)")
Parch <- ifelse(test$Parch==0,"No_ParCh","Have_ParCh")
Survived_Par<- ifelse(output$prediction ==1,"Survived","Dead")
table(Parch,Survived_Par)

counts <- table(Survived_Par,Parch)
barplot(counts, main="ParCh vs Survived",
        xlab="Has Parent/Child or not",
        ylab="Number of people",col=c("black","pink"),
        legend = rownames(counts))


SibSp <- ifelse(test$SibSp==0,"No_Sib./Sp.","Have_Sib./Sp.")
Survived_Sp <- ifelse(output$prediction ==1,"Survived","Dead")
table(SibSp,Survived_Sp)

counts <- table(Survived_Sp,SibSp)
barplot(counts, main="SibSp vs Survived",
        xlab="Has Sibling/Spouses or not",
        ylab="Number of people",col=c("black","orange"),
        legend = rownames(counts))


colnames(output)<-cbind("PassengerId","Survived")


write.csv(output, file = 'output_result.csv', row.names=F)