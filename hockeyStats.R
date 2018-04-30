splitStats <- read.csv("Split.csv")
seasonStats <- read.csv("Season.csv")
subSplit <- subset(splitStats, GP >= 11)
seasonSplit <- subset(seasonStats, GP >= 70)

What <- merge(subSplit,seasonSplit, by = "Player")
cutDown <- subset(What, select=c("Player", "GP.x", "Goals.x", "GP.y", "Goals.y"))
head(cutDown)
cutDown$ratio <- cutDown$Goals.x/cutDown$GP.x
cutDown$ratio
mean(cutDown$ratio)
