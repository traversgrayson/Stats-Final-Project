splitStats <- read.csv("Split.csv")
seasonStats <- read.csv("Season.csv")
gameStats <- read.csv("GamebyGame.csv")
subSplit <- subset(splitStats, iSF >= 22)
subSplit <- subset(subSplit, iSF <= 24 )
seasonSplit <- subset(seasonStats, GP >= 70)

What <- merge(subSplit,seasonSplit, by = "Player")
# t <- merge(What, gameStats, by = "Player")
cutDown2 <- subset(What, select=c("Player", "GP.x", "Goals.x", "GP.y", "Goals.y", "iSF.x"))







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
