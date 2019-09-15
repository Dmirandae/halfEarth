
asignarSpCeldas  <- function (vectorX,
                              numSpArea=1,
                              distNum="fix", 
                              paramNum=0.5){

    if(tolower(distNum)=="fix"){

        vectorX[sample(length(vectorX), numSpArea)]  <- 1

    }else{
		
        numSpArea  <- c(1 + as.integer(rexp(1,paramNum))) }    ## como mínimo 1, tambien puede cambiarse
        
        if( numSpArea > params$nSp ){
			numSpArea <- params$nSp  
			}
        
        vectorX[sample(length(vectorX), numSpArea)]  <- 1
     
      
        ## to use latter
#        spID[spID > numSpArea ]  <-  numSpArea
#        spID[spID <= 0 ]  <-  1
#        vectorX[spID]  <- 1

if (sum(vectorX) == 0){stop("Uhmm uhmmm")}
    
    return(vectorX)
}

