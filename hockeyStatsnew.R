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
seasonSplit <- subset(seasonStats, GP >= 50)

# Merge the split and season data and remove unnecessary columns
mergedData <- merge(subSplit,seasonSplit, by = "Player")
cutDownData <- subset(mergedData, select=c("Player", "GA.x", "SA.x", "GA.y", "SA.y", "Sv..x", "Sv..y"))
cutDownData$SvP.x <- cutDownData$Sv..x/100
cutDownData$SvP.y <- cutDownData$Sv..y/100
sum(cutDownData$SA.x)

# Caulculate the shrinkage c for the JS Estimator
k <- nrow(cutDownData) # number of unknown means
pbar <- mean(cutDownData$SvP.x)#total average of averages
n <- mean(cutDownData$SA.x) #average number of shots against
phat <- cutDownData$SvP.x # Sample means, the MLEs

c <- 1 - (k-3)*(pbar*(1 - pbar)/n)/sum((phat - pbar)^2) # apply the shrinkage formula
cAdjust <- mapply(min, c, 1)
cAdjust <- mapply(max,c, 0)
cAdjust

# Calculate our MSE values for JS Estimator and the MLE (SvP.x)
meanSq <- function(x, y){sqrt(mean((x-y)^2))}
cutDownData$JS <- pbar + cAdjust*(phat - pbar) # create a column for JS estimates
cutDownData$SMMS <- mapply(meanSq,cutDownData$SvP.x, cutDownData$SvP.y)
cutDownData$JSMS <- mapply(meanSq,cutDownData$JS, cutDownData$SvP.y)
meanSq(cutDownData$SvP.x, cutDownData$SvP.y) ## total MLE MSE 
meanSq(cutDownData$JS, cutDownData$SvP.y)
meanSq(pbar, cutDownData$SvP.y) ## total JS MSE

### ----------------------------------- ###

### ---------------------------  ###
###       Visualizations         ###
### ---------------------------  ###

# _________ #
### TABLE ###
# _________ #

tableData <- subset(cutDownData, select=c("Player","SvP.x","SvP.y","JS", "SMMS", "JSMS"))


### Code for side-by-side bar plots of MSE for JS and MLE ###
### ----------------------------------------------------- ###

cutDown <- subset(cutDownData, select=c("Player", "SMMS", "JSMS"))
cutDown$Player <- sub(".*? (.+)", "\\1", cutDown$Player)
cutDown.long<-melt(cutDown)
avMLE <- mean(cutDown$SMMS)
avJS <- mean(cutDown$JSMS)
ggplot(cutDown.long,aes(Player,value,fill=variable))+
  geom_point(stat="identity",position="dodge", aes(color = variable)) +
  theme(axis.text.x=element_text(angle = -55, hjust = 0)) +
  labs(title = "Mean Squared Error of MLE vs JS", y = "Mean Squared Error") +
  geom_hline(aes(yintercept = avMLE)) +
  geom_hline(aes(yintercept = avJS))
 
###     Code for Line-Plots inspired by Michael Lopez     ###
### ----------------------------------------------------- ###

cutDownSP <- subset(cutDownData, select=c("Player", "SvP.x", "JS", "SvP.y"))
cutDownSP$Player <- sub(".*? (.+)", "\\1", cutDownSP$Player)
cutDownSP <- cutDownSP[sample(nrow(cutDownSP),12),]
cutDownSP.long<-melt(cutDownSP)
SPmelt <- melt(cutDownSP, id="Player")
attach(SPmelt)
new <- SPmelt[order(SPmelt$Player),]
detach(SPmelt)

ggplot(data=new, aes(x=variable, y=value)) +
 geom_line(aes(group=Player, color=Player), linetype="solid") +
 geom_point(aes(color=Player)) +
 scale_x_discrete(labels=c("SvP.x" = expression(hat(p)['MLE']), "JS" = expression(hat(p)["JS"]),
                            "SvP.y" = expression('p'["season"]))) + 
  labs(title = "Visualization of Stein-Estimator", x = "Estimator", y = "Save Percentage")
#--------------------------------------------#

#ggplot(cutDownSP.long,aes(Player,value,fill=variable))+
 # geom_bar(stat="identity",position="dodge") +
  #theme(axis.text.x=element_text(angle = -55, hjust = 0)) +
  #coord_cartesian(ylim = c(.85, 1))
  #coord_flip() 

#SPplot <- cutDownSP > gather(key, value, -Player)
#ggplot(SPplot, mapping = aes(x = Player, y = value, color = key)) + geom_line()

# Grouped
#ggplot(cutDownData,aes(x=Player)) + 
# geom_bar(aes(y=SMMS),stat="identity",position="dodge",col="blue") +
#  geom_bar(position="dodge") +   
#geom_bar(aes(y=JSMS),stat="identity",position="dodge",col="yellow")
