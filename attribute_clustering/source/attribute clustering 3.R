srdataclusters <- srdata
srdataclusters <- cbind(srdataclusters,cluster=-1,centroid=-1)

assignclusterall <- function(){
  resultnames <<- data.frame(unique(srdata$result_name))
  
  for (i in 1:nrow(resultnames)){
    name <- as.character(resultnames[i,1])
    datarows <- nrow(subset(srdata,result_name==name))
    
    if (datarows > 5) {
      clusterinfo <- funit(name)
      
      for (j in 1:nrow(somedata)){
        attr_id <- somedata$attr_id[j]
        result_id <- somedata$result_id[j]
        cluster_nbr <- clusterinfo$cluster[j]
        centroid <- clusterinfo$centers[cluster_nbr,2]
        
        #update the master cluster info
        srdataclusters$centroid[srdataclusters$result_id==result_id & srdataclusters$attr_id==attr_id] <<- centroid
        srdataclusters$cluster[srdataclusters$result_id==result_id & srdataclusters$attr_id==attr_id] <<- cluster_nbr
      }   
    }
  }
}