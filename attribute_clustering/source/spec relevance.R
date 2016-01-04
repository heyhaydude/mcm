data <- read.csv("c:\\temp\\stats.txt",head=FALSE)
names(data) <- c("name","sd","count","spread","min","max")
data$SDbySpread <- data$sd / data$spread
data$SDbySpread[is.nan(data$SDbySpread)] <- 0
