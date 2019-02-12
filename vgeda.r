# EDA on vg sales data
vgdata <- read.csv("data/vg.csv")
summary(vgdata)

plot (vgdata$Global_Sales~vgdata$Year_of_Release)

plot(vgdata$Global_Sales~vgdata$Critic_Score)

plot(vgdata$Global_Sales~vgdata$NA_Sales)
nafit <- lm(vgdata$Global_Sales~vgdata$NA_Sales)
abline(nafit,col="red")
plot(vgdata$Global_Sales~vgdata$JP_Sales)


plot(vgdata$Global_Sales~vgdata$Platform)

plot(vgdata$Global_Sales~vgdata$Genre,main="Genre vs. Global Sales",las=2,log="y",xlab="Genre",ylab="Millions of Sales (logarithmic)")

head(vgdata$ order(vgdata$Global_Sales,decreasing=TRUE), n = 50)

#decision tree
smalldata <- head(vgdata,100)
#library(rpart)
#fit <- rpart(Gl ~ vgdata$Platform + vgdata$Rating,
#             method="class", data=vgdata)

require("randomForest")
require("MASS")
#train=sample(1:nrow(vgdata),300) #build training data
#randomForest(vgdata$Global_Sales ~ . , data = vgdata , na.action=na.roughfix, subset = train)

train=sample(1:nrow(Boston),300)
Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf
plot(Boston.rf)

