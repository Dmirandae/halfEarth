MST.Propincuity <- function (distance.Matrix){
  ##
  ## Based on ape::mst function
  ##

  
  if (class(distance.Matrix) == "dist") 
    distance.Matrix <- as.matrix(distance.Matrix)
  n <- dim(distance.Matrix)[1]
  
  tree <- listado <- NULL
  large.value <- max(distance.Matrix) + 1
  diag(distance.Matrix) <- large.value
  index.i  <- 1
  for (i in 1:(n - 1)) {
    tree <- c(tree, index.i)
    min.Distance <- apply(as.matrix(distance.Matrix[, tree]), 2, min)
    a <- sortIndex(distance.Matrix[, tree])[1, ]
    b <- sortIndex(min.Distance)[1]
    index.j <- tree[b]
    index.i <- a[b]
    
    listado[i] <- distance.Matrix[index.i, index.j]
    
    
    for (j in tree) {
      distance.Matrix[index.i, j] <- large.value
      distance.Matrix[j, index.i] <- large.value
    }
  }
  
  return(listado)
}

Propincuity.1 <- function (dist.Matrix) {
  
  ## forma 2 de calculo sin usar MST
  dist.Matrix <- as.matrix(distance) 
  
  large.value <- max(dist.Matrix) + 1
  diag(dist.Matrix) <- large.value
  
  size <- length(colnames(dist.Matrix))
  
  minimum <- NULL
  
  ## el arcercamiento puede sobre estimar la distancia entre puntos si ya el punto tiene una distancia minima en el 
  ## paquete y la segunda distancia es maslarga que esta anteiormente minima
  
  for (i in 1:size){
    
    minimum[i] <- min(dist.Matrix[i,])
    dist.Matrix[dist.Matrix == minimum[i]] <- large.value
    
  }
  
  return(minimum)
}

Propincuity.2 <- function (dist.Matrix) {
  
  # forma 3 usando los min de distancia
  
#~   dist.Matrix <- as.matrix(distance) 
  dist.Matrix <- as.matrix(dist.Matrix) 
  
  large.value <- max(dist.Matrix) + 1
  
  diag(dist.Matrix) <- large.value
  
  minimos <- apply(dist.Matrix,1,min)
  
  parejas.minimos <- matrix(NA,nrow =length(minimos),ncol = 2 )
  
  for (i in 1:length(minimos)){
    parejas.minimos[i,1] <- i
    parejas.minimos[i,2] <-(which(dist.Matrix[,i]==minimos[i]))
  }
  
  eliminar  <- NULL
  
  for (i in 1:(length(parejas.minimos[,1])-1)){
    for (j in (i+1):length(parejas.minimos[,1])){
      if (all(parejas.minimos[i,] %in% parejas.minimos[j,])){
#        print(paste("a eliminar en ",i,"---",j))
        parejas.minimos[j,] <-c(-rnorm(1),-rnorm(1))
        eliminar <- c(eliminar,j) 
      }
    }
  }
  
  minimos <- minimos[-eliminar]
  
  return(minimos)
  
}
