
asignarSpCeldas  <- function (vectorX,
                              numSpArea=1,
                              distNum="fix", 
                              paramNum=0.5){
							  
	vectorY <- vectorX
							  
	distNum <- tolower(distNum)

    if(distNum=="fix" | distNum=="0" ){

        vectorX[sample(length(vectorX), numSpArea)]  <- 1

    }else{
		
        numSpArea  <- c(1 + as.integer(rexp(1,paramNum))) }    ## como mínimo 1, tambien puede cambiarse
        
        #print(numSpArea)
        
        if( numSpArea > params$nSp ){
			numSpArea <- params$nSp  
			}
        
        vectorX[sample(length(vectorX), numSpArea)]  <- 1
     
      if(distNum=="2" ){
        ## to use later
        
        spID  <- c(1 + as.integer(rexp(params$SpEnArea,paramNum)))
        
        spID[spID > numSpArea ]  <-  numSpArea
        
        spID[spID <= 0 ]  <-  1
        
        #vectorX[spID]  <- 1
        
        vectorY[spID]  <- 1
        
        #print("t2")
        
        vectorX <- vectorY

}
if (sum(vectorX) == 0){stop("Uhmm uhmmm")}
    
    return(vectorX)
}

