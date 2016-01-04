
funit <- function(result_name,nbrofclusters) {
  
  if (missing(nbrofclusters)){
    nbrofclusters = 5
  }
  
  result_name <- tolower(result_name)
  result_id <- getattrid(result_name)
  print(paste(result_name, result_id, sep=" - ")) 
  
  somedata <<- getresultinfo(result_id)
  
  clusterdata <<- data.frame(somedata$x,somedata$score)
  names(clusterdata) <- c("x","score")
  clusterlength <- nrow(clusterdata)
  
  #png(filename=paste("C:\\temp\\attrimgs\\",result_name,".jpg"))
  #svg(filename=paste("C:\\temp\\attrimgs\\",result_name,".svg"))
  
  
  # setup plot area
  par(oma=c(0,0,2,0))
  par(mfrow=c(1,2))
    
  # cluster options and plot
  set.seed(35)
  cluster5results <- kmeans(clusterdata,nbrofclusters)
  plot(clusterdata,col=cluster5results$cluster)
  points(cluster5results$centers, col = 1:nbrofclusters, pch=3,cex=.75)
  title(sub = paste(nbrofclusters,"clusters",sep=" "),cex.sub = 0.75, font.sub = 3, col.sub = "black")
  centroids <- data.frame(cluster5results$centers[order(cluster5results$centers[,1]),])
  
  clusterdata$x <- 0
  set.seed(35)
  cluster5results <- kmeans(clusterdata,nbrofclusters)
  clusterdata$x <- seq.int(nrow(clusterdata)) - 1
  plot(clusterdata,col=cluster5results$cluster)
  points(cluster5results$centers, col = 1:nbrofclusters, pch=3,cex=.75)
  title(sub = paste(nbrofclusters,"clusters",sep=" "),cex.sub = 0.75, font.sub = 3, col.sub = "black")
  centroids <- cbind(centroids,data.frame(cluster5results$centers[order(cluster5results$centers[,1]),]))
  
  print(centroids)
  title(main=result_name,outer=T)
  
  #dev.off()
  
  return(cluster5results)
}