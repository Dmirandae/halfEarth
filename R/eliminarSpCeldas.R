
eliminarSpMatriz  <- function (matrixX,probThanos=params$propBorra){

mX <- matrixX

eliminarCeldas  <- function (vectorX,probThanos=params$propBorra){

#cat(probThanos,"\t**")

    if(runif(1) <= probThanos){

# print(vectorX[]) 
 
        vectorX[]  <- 0
 
# print(vectorX[]) 
    
}
    
    return(vectorX)
    
}    

for (contador in 1:params$nCells){
	
	matrixX[,contador] <- eliminarCeldas(matrixX[,contador])
	
	}

print(all(mX == matrixX))

    return(matrixX)
}
