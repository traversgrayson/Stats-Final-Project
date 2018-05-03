### ---------------------------------------------------- ###
### James-Stein Estimators for Save Percentage in Hockey ###
### ---------------------------------------------------- ###
### --- Lauren Caporilli and Travers Parsons-Grayson --- ###
###
### Credits: Much of the R code in this file was modeled off 
### of R code written by Professor Michael Lopez of Skidmore College 
### for a lecture on Stein's Paradox.
### URL: https://statsbylopez.files.wordpress.com/2016/01/lecture_8.pdf

### Goal: To use James-Stein Estimators to provide a better estimation of 
### end-of-season save percentages than the MLE

install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("reshape2")
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

### ----------------------------- ###
###         DATA ANALYSIS         ###
### ----------------------------- ###

### -------------------------------------- ###

splitStats <- read.csv("GoalieSplit.csv")
seasonStats <- read.csv("GoalieSeason.csv")
gameStats <- read.csv("GamebyGame.csv") 

# Keep only the players who have between 150-350 shots against in the first split, 
# and played at least 50 games in the season
subSplit <- subset(splitStats, SA >= 150)
subSplit <- subset(subSplit, SA <= 350)
seasonSplit <- subset(seasonStats, GP >= 40)

# Merge the split and season data and remove unnecessary columns
mergedData <- merge(subSplit,seasonSplit, by = "Player")
cutDownData <- subset(mergedData, select=c("Player", "GA.x", "SA.x", "GA.y", "SA.y", "Sv..x", "Sv..y"))
cutDownData$SvP.x <- cutDownData$Sv..x/100
cutDownData$SvP.y <- cutDownData$Sv..y/100

# Caulculate the shrinkage c for the JS Estimator
k <- nrow(cutDownData) # number of unknown means
p.bar <- 1 - mean(cutDownData$GA.x)/mean(cutDownData$SA.x) #total average of averages
p.hat <- cutDownData$SvP.x # Sample means, the MLEs
c <- 1 - (k-3)*(p.bar*(1 - p.bar)/cutDownData$SA.x)/sum((p.hat - p.bar)^2) # apply the shrinkage formula
c

# Calculate our MSE values for JS Estimator and the MLE (SvP.x)
meanSq <- function(x, y){sqrt(mean((x-y)^2))}
cutDownData$JS <- p.bar + c*(p.hat - p.bar) # create a column for JS estimates
cutDownData$SMMS <- mapply(meanSq,cutDownData$SvP.x, cutDownData$SvP.y)
cutDownData$JSMS <- mapply(meanSq,cutDownData$JS, cutDownData$SvP.y)
meanSq(cutDownData$SvP.x, cutDownData$SvP.y) ## total MLE MSE
meanSq(cutDownData$JS, cutDownData$SvP.y) ## total JS MSE
meanSq(p.bar, cutDownData$SvP.y)

### ----------------------------------- ###

### ---------------------------  ###
###       Visualizations         ###
### ---------------------------  ###


### Code for side-by-side bar plots of MSE for JS and MLE ###
### ----------------------------------------------------- ###

cutDown <- subset(cutDownData, select=c("Player", "SMMS", "JSMS"))
cutDown$Player <- sub(".*? (.+)", "\\1", cutDown$Player)
cutDown.long<-melt(cutDown)
ggplot(cutDown.long,aes(Player,value,fill=variable))+
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text.x=element_text(angle = -55, hjust = 0)) +
 scale_fill_manual(values=c("#E69F00", "#56B4E9"))

### ----------------------------------------------------- ###

cutDownSP <- subset(cutDownData, select=c("Player", "JS", "SvP.x", "SvP.y"))
cutDownSP$Player <- sub(".*? (.+)", "\\1", cutDownSP$Player)
cutDownSP.long<-melt(cutDownSP)
ggplot(cutDownSP.long,aes(Player,value,fill=variable))+
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text.x=element_text(angle = -55, hjust = 0)) +
  coord_cartesian(ylim = c(.85, 1))
  #coord_flip() 

#SPplot <- cutDownSP > gather(key, value, -Player)
#ggplot(SPplot, mapping = aes(x = Player, y = value, color = key)) + geom_line()

# Grouped
#ggplot(cutDownData,aes(x=Player)) + 
# geom_bar(aes(y=SMMS),stat="identity",position="dodge",col="blue") +
#  geom_bar(position="dodge") +   
#geom_bar(aes(y=JSMS),stat="identity",position="dodge",col="yellow")
