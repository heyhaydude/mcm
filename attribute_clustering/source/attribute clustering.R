options("scipen" = 15)

getattrid <- function(name) {
  tempid <- -1
  matches <- subset(srdata,result_name==tolower(name))
  if (nrow(matches) > 0) {
    tempid <- matches$result_id[1]
  }
  return(tempid)
}

getresultinfo <- function(id) {
  idnum <- as.numeric(id)
  matches <- subset(srdata,result_id==idnum)
  return(matches)
}

srdata <- read.csv("C:\\Users\\Matt\\Desktop\\Files\\dev\\mcm\\attribute_clustering\\data\\attrclusterinfo.txt",head=FALSE, quote="",
                  colClasses = c("numeric","numeric","character","numeric","character","numeric"))
names(srdata) <- c("x","result_id","result_name","attr_id","attr_name","score")
srdata$result_name <- tolower(srdata$result_name)
srdata$attr_name <- tolower(srdata$attr_name)

clusterdata <- data.frame()
somedata <-data.frame()

getattr <- function(pos){
  return(somedata$attr_name[somedata$x==pos])
}

runit <- function(result_name) {
  
  #result_name <- 'hand scrapers'
  result_name <- tolower(result_name)
  result_id <- getattrid(result_name)
  print(paste(result_name, result_id, sep=" - ")) 
  
  somedata <<- getresultinfo(result_id)
  
  plot(somedata$x,somedata$score)
  
  clusterdata <<- data.frame(somedata$x,somedata$score)
  names(clusterdata) <- c("x","score")
  clusterlength <- nrow(clusterdata)
  
  # setup plot area
  if (clusterlength >= 5){
    par(oma=c(0,0,2,0))
    par(mfrow=c(2,2))
  } else if (clusterlength >= 3) {
    par(mfrow=c(1,2))
  } else {
    par(mfrow=c(1,1))
  }
  
  
  # cluster options and plot
  if (clusterlength >= 5){
    set.seed(35)
    cluster5results <- kmeans(clusterdata,5)
    plot(clusterdata,col=cluster5results$cluster)
    points(cluster5results$centers, col = 1:5, pch=3,cex=.75)
    title(sub = "5 clusters",cex.sub = 0.75, font.sub = 3, col.sub = "black")
  }
  if (clusterlength >= 4){
    set.seed(35)
    cluster4results <- kmeans(clusterdata,4)  
    plot(clusterdata,col=cluster4results$cluster)
    points(cluster4results$centers, col = 1:4, pch=3,cex=.75)
    title(sub = "4 clusters",cex.sub = 0.75, font.sub = 3, col.sub = "black")
  }
  if (clusterlength >= 3){
    set.seed(35)
    cluster3results <- kmeans(clusterdata,3)  
    plot(clusterdata,col=cluster3results$cluster)
    points(cluster3results$centers, col = 1:3, pch=3,cex=.75)
    title(sub = "3 clusters",cex.sub = 0.75, font.sub = 3, col.sub = "black")
  }
  if (clusterlength >= 2){
    set.seed(35)
    cluster2results <- kmeans(clusterdata,2)
    plot(clusterdata,col=cluster2results$cluster)
    points(cluster2results$centers, col = 1:2, pch=3,cex=.75)
    title(sub = "2 clusters",cex.sub = 0.75, font.sub = 3, col.sub = "black")
  }
  
  print(cluster5results$centers[order(cluster5results$centers[,1]),])
  #mtext('Attribute Clustering', outer = TRUE, cex = 1)
  title(main=result_name,outer=T)
  

}

#clusterdata$somedata.x <- seq.int(nrow(clusterdata)) - 1


# srdata_protects <- getattrinfo(9999946122922)
# cldprotects <- data.frame(srdata_protects$x,srdata_protects$click_count)
# names(cldprotects) <- c("x","click_count")
# #cldprotects <- subset(cldprotects,click_count > 0)
# #cldprotects$click_count <- log(cldprotects$click_count)
# cl_protects2 <- kmeans(cldprotects,2)
# cl_protects3 <- kmeans(cldprotects,3)
# cl_protects4 <- kmeans(cldprotects,4)
# cl_protects5 <- kmeans(cldprotects,5)
# par(mfrow=c(2,2))
# plot(cldprotects,col=cl_protects2$cluster)
# points(cl_protects2$centers, col = 1:2, pch=20,cex=2)
# plot(cldprotects,col=cl_protects3$cluster)
# points(cl_protects3$centers, col = 1:3, pch=20,cex=2)
# plot(cldprotects,col=cl_protects4$cluster)
# points(cl_protects4$centers, col = 1:4, pch=20,cex=2)
# plot(cldprotects,col=cl_protects5$cluster)
# points(cl_protects5$centers, col = 1:5, pch=20,cex=2)
