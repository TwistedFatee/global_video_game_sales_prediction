### This script will clean, and discretize dataset
if (!require("rstudioapi")){
  install.packages("rstudioapi")
  library("rstudioapi")
}


#Set Working directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))


vgdata <- read.csv("data/vg.csv", na.strings=c("","NA"),stringsAsFactors = FALSE)

#Create a function to identify platforms.
getPlatform <- function(name){
  ninPlats <- c("NES","SNES","GB","GBA","DS","3DS","N64","GC","Wii","WiiU","Switch")
  sonyPlats <- c("PS","PS2","PS3","PS4")
  msPlats <- c("XB","X360","XOne")
  
  if (is.element(name,ninPlats)){return ("Nintendo")}
  else if (name %in% sonyPlats){ return ("Sony")}
  else if (name %in% msPlats){ return ("Microsoft")}
  else if (name == "PC"){return ("PC")}
  else return ("Other")
}


#Remove huge outliers, specifically, Wii Sports at 80m sales.
no.outliers <- vgdata$Global_Sales[vgdata$Global_Sales < 20]
#Take the mean of this temporary dataset.
#This mean will be used for discretizing the sales into low, medium, and high
gs.mean <- mean(no.outliers)


#The range of medium sales is +/- 10% of the mean
meanrange.lower <- 0.8
meanrange.upper <- 1.2

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

cnames <- c("Name","Sales","UserScore","Developer","CriticalScore","Genre","Platform","Rating")
gameclass <- data.frame(matrix(NA, nrow = 16719, ncol = 8))
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

#Platforms
vec <- sapply(vgdata$Platform,getPlatform)
gameclass$Platform = vec

#Rating

pts <- which(is.na(vgdata$Rating))
gameclass$Rating = vgdata$Rating
gameclass$Rating[pts] = "T" #Give a default undefined rating of T (Teen) as a median

#Critic scores
low_scores <- which(gameclass$UserScore < score_avg_lower)
high_scores <- which(gameclass$UserScore > score_avg_upper)
med_scores <- which(gameclass$UserScore >= score_avg_lower & gameclass$UserScore <= score_avg_upper)
gameclass$UserScore[high_scores] = "High"
gameclass$UserScore[med_scores] = "Medium"
gameclass$UserScore[low_scores] = "Low"


write.csv(gameclass,"./data/cleaned.csv")

