

eliminarSpCeldas  <- function (matrixX,probThanos=0.5){

eliminarCeldas  <- function (vectorX,probThanos=0.5){

    if(runif(1) < probThanos){

        vectorX[]  <- 0
    
}
    
    return(vectorX)
    
}    

for (contador in 1:params$nCells){
	
	matrixX[contador,] <- eliminarCeldas(matrixX[contador,])
	
	}

    return(matrixX)
}
