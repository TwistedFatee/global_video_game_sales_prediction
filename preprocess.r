### This script will clean, and discretize dataset
if (!require("rstudioapi")){
  install.packages("rstudioapi")
  library("rstudioapi")
}
#Set Working directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))


vgdata <- read.csv("data/vg.csv", na.strings=c("","NA"),stringsAsFactors = FALSE)

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

#Missing values in User_Score
    user.mean <- mean(as.numeric(vgdata$User_Score), na.rm = TRUE)
    user.nalocs <- which(is.na(as.numeric(vgdata$User_Score)))

#### Create new dataset containing our preprocessed data.

cnames <- c("Name","Sales","CriticalScore","UserScore","Genre","Developer")
gameclass <- data.frame(matrix(NA, nrow = 16719, ncol = 6))
colnames(gameclass) <- cnames

#Transfer existing columns
gameclass$Name = vgdata$Name
gameclass$Genre = vgdata$Genre
gameclass$Developer = vgdata$Developer
gameclass$CriticalScore = vgdata$Critic_Score
gameclass$UserScore = vgdata$User_Score

#Fill in missing data
genre_default <- "Other"
dev_default <- "Other"


gameclass$CriticalScore[critic.nalocs] = critic.mean
gameclass$UserScore[user.nalocs] = user.mean
gameclass$Sales[which(is.na(gameclass$Sales))] = "Medium"
gameclass$Genre[which(is.na(gameclass$Genre))] = genre_default
gameclass$Developer[which(is.na(gameclass$Developer))] = dev_default





##Assign high/med/low classification for sales.
gameclass$Sales[high.sales] = "High"
gameclass$Sales[med.sales] = "Medium"
gameclass$Sales[low.sales] = "Low"

##Discretize the scores

#Range for low/med/hi +/- 10% of average
score_avg_upper <- critic.mean*1.1
score_avg_lower <- critic.mean*0.9

#Critic scores


low_scores <- which(gameclass$CriticalScore < score_avg_lower)
high_scores <- which(gameclass$CriticalScore > score_avg_upper)
med_scores <- which(gameclass$CriticalScore >= score_avg_lower & gameclass$CriticalScore <= score_avg_upper)
gameclass$CriticalScore[high_scores] = "High"
gameclass$CriticalScore[med_scores] = "Medium"
gameclass$CriticalScore[low_scores] = "Low"

# User Scores

score_avg_upper <- critic.mean*1.1
score_avg_lower <- critic.mean*0.9

#Critic scores
low_scores <- which(gameclass$UserScore < score_avg_lower)
high_scores <- which(gameclass$UserScore > score_avg_upper)
med_scores <- which(gameclass$UserScore >= score_avg_lower & gameclass$UserScore <= score_avg_upper)
gameclass$UserScore[high_scores] = "High"
gameclass$UserScore[med_scores] = "Medium"
gameclass$UserScore[low_scores] = "Low"


write.csv(gameclass,"./data/cleaned.csv")

