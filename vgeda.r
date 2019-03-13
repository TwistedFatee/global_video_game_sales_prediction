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


eufit<- lm(vgdata$Global_Sales~vgdata$EU_Sales)
plot(vgdata$Global_Sales~vgdata$EU_Sales)
abline(eufit, col="red")


jpfit<- lm(vgdata$Global_Sales~vgdata$JP_Sales)
plot(vgdata$Global_Sales~vgdata$JP_Sales,log="x")
abline(jpfit, col="red")
boxplot(vgdata$JP_Sales)

minmax_norm <- function(data, minval, maxval){
 return ((data-min(data))/(max(data)-min(data)))*((maxval-minval)+minval)
}

znorm <- function (data){
    return (data - mean(data))/sd(data)
}

gs <- minmax_norm(vgdata$Global_Sales,0,1)
ns <- minmax_norm(vgdata$NA_Sales,0,1)

glob <- znorm(vgdata$Global_Sales)
north <- znorm(vgdata$NA_Sales)
plot(glob~north)
temp <- vgdata$Global_Sales * 1000000


library(e1071)   
skewness(gs)
plot(gs[gs <= 0.05])

nn <- minmax_norm(temp,0,1)
skewness(nn)
plot(density(nn))
plot(density(log(nn)))
plot(density(log(vgdata$Global_Sales)))
plot(density(log(vgdata$Global_Sales[vgdata$Global_Sales <= 20])))
lnn <- log(nn)

plot(density(scale(gs)))
#/*
#minval <- 0
#maxval <- 1
#minmaxGS <- (vgdata$Global_Sales - max(vgdata$Global_Sales))/(max(vgdata$Global_Sales)-min(vgdata$Global_Sales))*
#            (maxval-minval)+minval
#
plot (gs~ns)

minmax_norm(vgdata)


