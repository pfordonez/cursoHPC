library(profvis)
library(dlnm)
datafr <- chicagoNMMAPS
threshold <- 25

profvis({
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold & 
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
})


####Probando con Paralel

rm(list=ls(all=TRUE))
gc()
require(parallel)
data <- read.csv( 'dataset.csv' )
parallel.function <- function(i) {
  kmeans( data, centers=4, nstart=i )
}

cl <- makeForkCluster(12L)  # Equivalente makeCluster(4L, type= "FORK")
clusterSetRNGStream(cl, 123) 

profvis({
  time_parLapply<-system.time({  
    results <- parallel::parLapply( cl, c(1,1,5,5,2,2,1,1), fun=parallel.function )
    temp.vector <- sapply( results, function(result) { result$tot.withinss } )
    result <- results[[which.min(temp.vector)]]
    print(result$tot.withinss)
  })
})
