
matrizLimpia  <- function(numSp=5,numCells=10){

mLimpia  <-  matrix(data=0, nrow=numSp, ncol=numCells, byrow=TRUE,
                        dimnames = list(paste("sp",1:numSp,sep=""),
                                        paste("Area",1:numCells,sep="")
                                        )) 
return(mLimpia)

}
