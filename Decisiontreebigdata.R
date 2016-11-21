library(ff)
library(ffbase)
library(e1071)
library(arules)
library(MASS)
library(tree)
library(party)
library(rpart)


data = read.csv("census2.csv",
                sep=",",header=FALSE,col.names=c("age", "type_employer", "fnlwgt", "education", 
                                                 "education_num","marital", "occupation", "relationship", "race","sex",
                                                 "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                fill=FALSE,strip.white=T)




data[["education_num"]]=NULL
data[["fnlwgt"]]=NULL

data$type_employer = as.character(data$type_employer)
data$occupation = as.character(data$occupation)
data$country = as.character(data$country)
data$education = as.character(data$education)
data$race = as.character(data$race)
data$marital = as.character(data$marital)

table(data$type_employer)
employer=data$type_employer
employer.freq=table(employer)
hist(employer.freq)

data$type_employer = gsub("^Federal-gov","Federal-Govt",data$type_employer)
data$type_employer = gsub("^Local-gov","Other-Govt",data$type_employer)
data$type_employer = gsub("^State-gov","Other-Govt",data$type_employer)
data$type_employer = gsub("^Private","Private",data$type_employer)
data$type_employer = gsub("^Self-emp-inc","Self-Employed",data$type_employer)
data$type_employer = gsub("^Self-emp-not-inc","Self-Employed",data$type_employer)
data$type_employer = gsub("^Without-pay","Not-Working",data$type_employer)
data$type_employer = gsub("^Never-worked","Not-Working",data$type_employer)

table(data$occupation)

data$occupation = gsub("^Adm-clerical","Admin",data$occupation)
data$occupation = gsub("^Armed-Forces","Military",data$occupation)
data$occupation = gsub("^Craft-repair","Blue-Collar",data$occupation)
data$occupation = gsub("^Exec-managerial","White-Collar",data$occupation)
data$occupation = gsub("^Farming-fishing","Blue-Collar",data$occupation)
data$occupation = gsub("^Handlers-cleaners","Blue-Collar",data$occupation)
data$occupation = gsub("^Machine-op-inspct","Blue-Collar",data$occupation)
data$occupation = gsub("^Other-service","Service",data$occupation)
data$occupation = gsub("^Priv-house-serv","Service",data$occupation)
data$occupation = gsub("^Prof-specialty","Professional",data$occupation)
data$occupation = gsub("^Protective-serv","Other-Occupations",data$occupation)
data$occupation = gsub("^Sales","Sales",data$occupation)
data$occupation = gsub("^Tech-support","Other-Occupations",data$occupation)
data$occupation = gsub("^Transport-moving","Blue-Collar",data$occupation)

table(data$country)

data$country[data$country=="Cambodia"] = "SE-Asia"
data$country[data$country=="Canada"] = "British-Commonwealth"    
data$country[data$country=="China"] = "China"       
data$country[data$country=="Columbia"] = "South-America"    
data$country[data$country=="Cuba"] = "Other"        
data$country[data$country=="Dominican-Republic"] = "Latin-America"
data$country[data$country=="Ecuador"] = "South-America"     
data$country[data$country=="El-Salvador"] = "South-America" 
data$country[data$country=="England"] = "British-Commonwealth"
data$country[data$country=="France"] = "Euro_1"
data$country[data$country=="Germany"] = "Euro_1"
data$country[data$country=="Greece"] = "Euro_2"
data$country[data$country=="Guatemala"] = "Latin-America"
data$country[data$country=="Haiti"] = "Latin-America"
data$country[data$country=="Holand-Netherlands"] = "Euro_1"
data$country[data$country=="Honduras"] = "Latin-America"
data$country[data$country=="Hong"] = "China"
data$country[data$country=="Hungary"] = "Euro_2"
data$country[data$country=="India"] = "British-Commonwealth"
data$country[data$country=="Iran"] = "Other"
data$country[data$country=="Ireland"] = "British-Commonwealth"
data$country[data$country=="Italy"] = "Euro_1"
data$country[data$country=="Jamaica"] = "Latin-America"
data$country[data$country=="Japan"] = "Other"
data$country[data$country=="Laos"] = "SE-Asia"
data$country[data$country=="Mexico"] = "Latin-America"
data$country[data$country=="Nicaragua"] = "Latin-America"
data$country[data$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
data$country[data$country=="Peru"] = "South-America"
data$country[data$country=="Philippines"] = "SE-Asia"
data$country[data$country=="Poland"] = "Euro_2"
data$country[data$country=="Portugal"] = "Euro_2"
data$country[data$country=="Puerto-Rico"] = "Latin-America"
data$country[data$country=="Scotland"] = "British-Commonwealth"
data$country[data$country=="South"] = "Euro_2"
data$country[data$country=="Taiwan"] = "China"
data$country[data$country=="Thailand"] = "SE-Asia"
data$country[data$country=="Trinadad&Tobago"] = "Latin-America"
data$country[data$country=="United-States"] = "United-States"
data$country[data$country=="Vietnam"] = "SE-Asia"
data$country[data$country=="Yugoslavia"] = "Euro_2"

data$education = gsub("^10th","Dropout",data$education)
data$education = gsub("^11th","Dropout",data$education)
data$education = gsub("^12th","Dropout",data$education)
data$education = gsub("^1st-4th","Dropout",data$education)
data$education = gsub("^5th-6th","Dropout",data$education)
data$education = gsub("^7th-8th","Dropout",data$education)
data$education = gsub("^9th","Dropout",data$education)
data$education = gsub("^Assoc-acdm","Associates",data$education)
data$education = gsub("^Assoc-voc","Associates",data$education)
data$education = gsub("^Bachelors","Bachelors",data$education)
data$education = gsub("^Doctorate","Doctorate",data$education)
data$education = gsub("^HS-Grad","HS-Graduate",data$education)
data$education = gsub("^Masters","Masters",data$education)
data$education = gsub("^Preschool","Dropout",data$education)
data$education = gsub("^Prof-school","Prof-School",data$education)
data$education = gsub("^Some-college","HS-Graduate",data$education)

data$marital[data$marital=="Never-married"] = "Never-Married"
data$marital[data$marital=="Married-AF-spouse"] = "Married"
data$marital[data$marital=="Married-civ-spouse"] = "Married"
data$marital[data$marital=="Married-spouse-absent"] = "Not-Married"
data$marital[data$marital=="Separated"] = "Not-Married"
data$marital[data$marital=="Divorced"] = "Not-Married"
data$marital[data$marital=="Widowed"] = "Widowed"

data$race[data$race=="White"] = "White"
data$race[data$race=="Black"] = "Black"
data$race[data$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
data$race[data$race=="Asian-Pac-Islander"] = "Asian"
data$race[data$race=="Other"] = "Other"

#data[["capital_gain"]] <- ordered(cut(data$capital_gain,c(-Inf, 0, 
 #                                                         median(data[["capital_gain"]][data[["capital_gain"]] >0]), 
  #                                                        Inf)),labels = c("None", "Low", "High"))
#data[["capital_loss"]] <- ordered(cut(data$capital_loss,c(-Inf, 0, 
 #                                                         median(data[["capital_loss"]][data[["capital_loss"]] >0]), 
  #                                                        Inf)), labels = c("None", "Low", "High"))

is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)

data$marital = factor(data$marital)

data$education = factor(data$education)
data$country = factor(data$country)
data$type_employer = factor(data$type_employer)
data$occupation = factor(data$occupation)
data$race = factor(data$race)
data$sex = factor(data$sex)
data$relationship = factor(data$relationship)
#data$income = as.factor(ifelse(data$income==data$income[1],0,1))
data$income = factor(data$income)
data$age=factor(data$age)
data$hr_per_week = factor(data$hr_per_week)
#data$age = scale(data$age)
#data$hr_per_week = scale(data$hr_per_week)
summary(data)
writefu<-write.table(data, file = "MyData.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
#writefu<- write.csv(data,"data1.csv")
# Sampling the data
sample = rbinom(dim(data)[1],1,.3)
#Training dataset
trainset = data[sample==0,]   
#Test dataset
valset = data[sample==1,]

# Naive Bayes Implementation
model=naiveBayes(income~.,data=trainset)
model$apriori
#pred<-predict(model,valset)==valset$income
pred<-predict(model,valset)
summary(pred)
confusionMatrix(pred,valset$income)

#Apriori Algorithm(Association Rules)

rules <- apriori(data, parameter = list(supp = 0.5, conf = 0.70))
summary(rules)
inspect(rules)

# Using FF package to read the MyData.csv 
#Decision tree Implementation
fullData<- read.csv.ffdf(file="MyData.csv", header=TRUE, VERBOSE=TRUE, first.rows=1000, next.rows=50000, colClasses=NA)
names(fullData) <- c("age", "workclass", "education", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "response")
set.seed(100)
train <- sample (1:nrow(fullData), .8*nrow(fullData)) # training row indices
inputData <- fullData[train, ] # training data
testData <- fullData[-train, ] # test data
treeMod <- tree(response ~ ., data = inputData)  # model the tree, including all the variables
plot(treeMod)  # Plot the tree model
text(treeMod, pretty=0)  # Add text to the plot
out <- predict(treeMod) # Predict the training data
input.response <- as.character(inputData$response) # actuals
pred.response <- colnames(out)[max.col(out, ties.method = c("first"))] # predicted
mean (input.response != pred.response) # misclassification %
cvTree <- cv.tree(treeMod, FUN = prune.misclass)  # run the cross validation
plot(cvTree)  # plot the CV
treePrunedMod <- prune.misclass(treeMod, best = 9) # set size corresponding to lowest value in below plot. try 4 or 16.
plot(treePrunedMod)
text(treePrunedMod, pretty = 0)
out <- predict(treePrunedMod) # fit the pruned tree
pred.response <- colnames(out)[max.col(out, ties.method = c("random"))] # predicted
mean(inputData$response != pred.response) # Calculate Mis-classification error.
fit <- ctree (response ~ ., data = inputData)  # build the tree model
plot (fit, main="Conditional Inference Tree")  # the ctree
pred.response <- as.character (predict(fit), testData) # predict on test data
input.response <- as.character (testData$response) # actuals
mean (input.response != pred.response) # misclassification %
rpartMod <- rpart(response ~ ., data = inputData, method = "class")  # build the mode
printcp(rpartMod)  # print the cptable
out <- predict(rpartMod) # predict probabilities
pred.response <- colnames(out)[max.col(out, ties.method = c("random"))] # predict response
mean(inputData$response != pred.response) # % misclassification error 
out <- predict(rpartMod, testData)


