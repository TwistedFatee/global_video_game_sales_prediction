if (!require("rstudioapi")){
  install.packages("rstudioapi")
  library("rstudioapi")
}

if (!require("randomForest")){
  install.packages("randomForest")
  library("randomForest")
}

if (!require("party")){
  install.packages("party")
  library("party")
}



#TODO: add cross validate support 
create_testset <- function(data, size = 0.8, train = TRUE){
  n_row <- nrow(data)
  total_Row <- size * n_row
  sample <- 1:total_Row
  if (train){
      return (data[sample,])
  } else {
      return (data[-sample,])
  }
}

#Set Working directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

gamedata <- read.csv("data/cleaned.csv")

training_data <- create_testset(gamedata, train = TRUE)
test_data <- create_testset(gamedata ,train = FALSE)

xx <- gamedata[,6:9]
yy <- gamedata[,3]
#game.rf <- randomForest(Sales ~ CriticalScore + Genre + Platform + Rating,data=gamedata, importance=FALSE,proximity=FALSE,na.action=na.omit, nodesize=16)

#plot(game.rf, log="y")
#varImpPlot(game.rf)

#visreg(game.rf,"CriticalScore",ylab="Sales")

ntree <- 5
library("party")
cf <- cforest(Sales ~ CriticalScore + Genre + Platform + Rating, data=gamedata,controls=cforest_control(ntree=ntree))
#ct <- ctree(Sales ~ CriticalScore + Genre + Platform + Rating, data=gamedata,controls=cforest_control(ntree=ntree))

#png("tree.png", res=80, height=3200, width=3200) 
#plot(ct, type="simple",
'     inner_panel=node_inner(ct,
                            abbreviate = TRUE,            # short variable names
                            pval = FALSE,                 # no p-values
                            id = FALSE),                  # no id of node
     terminal_panel=node_terminal(ct, 
                                  abbreviate = TRUE,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = TRUE)
)'
#dev.off()



#ct2 <- ctree(Sales ~ Genre + Rating, data=gamedata)
#plot(ct2)
#dev.off()
#for(i in 1:ntree){
#  pt <- prettytree(cf@ensemble[[i]], names(cf@data@get("input"))) 
 # nt <- new("Random Forest BinaryTree") 
#  nt@tree <- pt 
#  nt@data <- cf@data 
#  nt@responses <- cf@responses 
  
#  pdf(file=paste0("filex",i,".pdf"))
#  plot(pt, type="simple")
#  dev.off()
  
#}

