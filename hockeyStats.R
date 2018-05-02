

splitStats <- read.csv("GoalieSplit.csv")
seasonStats <- read.csv("GoalieSeason.csv")
gameStats <- read.csv("GamebyGame.csv")
subSplit <- subset(splitStats, SA >= 150)
subSplisubSplit <- subset(subSplit, SA <= 350)
seasonSplit <- subset(seasonStats, GP >= 50)

mergedData <- merge(subSplit,seasonSplit, by = "Player")
# t <- merge(What, gameStats, by = "Player")
cutDownData <- subset(mergedData, select=c("Player", "GA.x", "SA.x", "GA.y", "SA.y", "Sv..x", "Sv..y"))

k <- nrow(cutDownData)

p.bar <- 1 - mean(cutDownData$GA.x)/mean(cutDownData$SA.x)
p.bar
p.hat <- cutDownData$Sv..x/100
p.hat

ss.p.bar <- sum((p.hat - p.bar)^2)
ss.p.bar

sig.squared <- p.bar*(1 - p.bar)/cutDownData$SA.x
sig.squared

c <- 1 - (k-3)*(p.bar*(1 - p.bar)/cutDownData$SA.x)/sum((p.hat - p.bar)^2)
c
cutDownData$JS <- p.bar + c*(p.hat - p.bar)
RMSE <- function(x, y){sqrt(mean((x-y)^2))}
cutDownData$SvP.x <- cutDownData$Sv..x/100
cutDownData$SvP.y <- cutDownData$Sv..y/100

cutDownData$SMMS <- mapply(RMSE,cutDownData$SvP.x, cutDownData$SvP.y)
cutDownData$JSMS <- mapply(RMSE,cutDownData$JS, cutDownData$SvP.y)
RMSE(cutDownData$SvP.x, cutDownData$SvP.y)
RMSE(cutDownData$JS, cutDownData$SvP.y)
RMSE(p.bar, cutDownData$SvP.y)

counts <- table(cutDownData$JSMS, cutDownData$SMMS)
counts <- counts[!is.na[counts]]
barplot(cutDownData$JS, beside = TRUE,space = 1, names.arg = cutDownData$Player)



# cutDown <- subset(t, select=c("Player", "GP.x", "Goals.x", "GP.y", "Goals.y", "Goals"))
# head(cutDown,12)
# cutDown2$ratio <- cutDown2$Goals.x/cutDown2$GP.x
# cutDown$ratio
# var(cutDown$ratio)
# mean(cutDown$ratio)
# mom <- by(cutDown$Goals, cutDown$Player, mean)
# dad <- by(cutDown$Goals, cutDown$Player, var)
# spliff <- split(cutDown, cutDown$Player)
# but <- sapply(split.Goals, mean)
# 
# values <- dad[!is.na(dad)]
# values
# head(values)
# 
# shrinkage <- c()
# for (i in 1:length(values)) {
#   shrinkage[i] = 1 - (79 * 0.02421146/sum((cutDown2$ratio - mean(cutDown2$ratio))^2))}
# 
# mean(cutDown$Goals)
# var(cutDown$Goals)
