
matrizLimpia  <- function(nSp=5,nCells=10){

mLimpia  <-  matrix(data=0, nrow=nSp, ncol=nCells, byrow=TRUE,
                        dimnames = list(paste("sp",1:nSp,sep=""),
                                        paste("Area",1:nCells,sep="")
                                        )) 
return(mLimpia)

}
