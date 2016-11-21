library(corrplot)  ## package for plotting correlations
library(prettyR)   ## package for cross-tabulation
library(ggplot2)   ## package for producing graphical plots
library(gridExtra) ## package for drawing multiple graphs on a grid
 
data<-read.csv("census.csv",header = TRUE,sep = ",")
par(mfrow=c(1,1))
par(las=1)  ## horizontal axis labels
plot(table(data$Income, data$Type_Employer), main="Type_Employer vs. Income Class", cex=1.5)
plot(table(data$Income, data$Marital), main="Marital Status vs. Income Class", cex=1.5)
plot(table(data$Income, data$occupation), main="Occupation vs. Income Class", cex=1.5)
plot(table(data$Income, data$Relationship), main="Relationship vs. Income Class", cex=1.5)
plot(table(data$Income, data$Race), main="Race vs. Income Class", cex=1.5)
plot(table(data$Income, data$sex), main="Sex vs. Income Class", cex=1.5)
#plot(table(data$Income, data$Age), main="Age vs. Income Class", cex=1.5)
plot(table(data$Income, data$Education), main="Education vs. Income Class", cex=1.5)
plot(table(data$Income, data$Country), main="Country vs. Income Class", cex=1.5)
plot(table(data$Income, data$Capital_gain), main="Capital_gain vs. Income Class", cex=1.5)
plot(table(data$Income, data$Capital_loss), main="Capital_loss vs. Income Class", cex=1.5)
plot(table(data$Income, data$Hour_per_week), main="Hour_per_week vs. Income Class", cex=1.5)

par(las=0)  ## parallel axis labels

Capital_gain=data$Capital_gain
Capital_gain.freq=table(Capital_gain)
pie((Capital_gain.freq),main = "Capital_gain")


Capital_loss=data$Capital_loss
Capital_loss.freq=table(Capital_loss)
pie((Capital_loss.freq),main = "Capital_loss")

Hour_per_week=data$Hour_per_week
Hour_per_week.freq=table(Hour_per_week)
pie((Hour_per_week.freq),main = "Hour_per_week")