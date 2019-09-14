
asignarSpCeldas  <- function (vectorX,numSpEnArea=1,
                              #nSp=nSp,
                              distNum="fix", paramNum=0.5){
    if(tolower(distNum)=="fix"){

        vectorX[sample(length(vectorX), numSpEnArea)]  <- 1

    }else{

        numSpEnArea  <- c(1 + as.integer(rexp(1,paramNum))) ## como mínimo 1, tambien puede cambiarse
        
        if( numSpEnArea > nSp ){
			numSpEnArea <- nSp  
			}

        ##cat("\n**\tnumSpEnArea",numSpEnArea,"\n") ## para revisar num sp
        
        vectorX[sample(length(vectorX), numSpEnArea)]  <- 1

    } 
        
        ## estos los usare mas adelante para simular distribuciones espaciales heterogeneas, no les gaste gasolina es para tenerlas en mente

#        spID[spID > numSpEnArea ]  <-  numSpEnArea
#        spID[spID <= 0 ]  <-  1
#        vectorX[spID]  <- 1

    
    return(vectorX)
}

