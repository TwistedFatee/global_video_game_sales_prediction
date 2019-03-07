# Clean, and discretize dataset

vgdata <- read.csv("data/vg.csv")

#Remove huge outliers, specifically, Wii Sports at 80m sales.
no.outliers <- vgdata$Global_Sales[vgdata$Global_Sales < 20]
#Take the mean of this temporary dataset.
#This mean will be used for discretizing the sales into low, medium, and high
gs.mean <- mean(no.outliers)

#The range of medium sales is +/- 10% of the mean
meanrange.lower <- 0.9
meanrange.upper <- 1.1

low.sales <- which(vgdata$Global_Sales < gs.mean*meanrange.lower)
med.sales <- which(vgdata$Global_Sales >= gs.mean*meanrange.lower & vgdata$Global_Sales <= gs.mean*meanrange.upper)
high.sales <- which(vgdata$Global_Sales > meanrange.upper)

###Clean up missing values

#Missing Values in Critic_Score
#Take the mean, disregarding NA values.
critic.mean <- mean(vgdata$Critic_Score, na.rm = TRUE)
critic.nalocs <- which(is.na(vgdata$Critic_Score))

#TODO: do this for the other categories.

#### Create new dataset containing our preprocessed data.

cnames <- c("Name","Sales","CriticalScore","UserScore","Genre","Developer")
gameclass <- data.frame(matrix(NA, nrowt = 16719, ncol = 6))
colnames(gameclass) <- cnames

#Transfer existing columns
gameclass$Name = vgdata$Name
gameclass$Genre = vgdata$Genre
gameclass$Developer = vgdata$Developer
gameclass$CriticalScore = vgdata$Critic_Score
gameclass$UserScore = vgdata$User_Score

#Assign high/med/low classification.
gameclass$Sales[high.sales] = "High"
gameclass$Sales[med.sales] = "Medium"
gameclass$Sales[low.sales] = "Low"

#Fill in missing data
gameclass$CriticalScore[critic.nalocs] = critic.mean
gameclass$Sales[which(is.na(gameclass$Sales))] = "Medium"



