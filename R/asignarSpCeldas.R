
asignarSpCeldas  <- function (vectorX,
                              numSpArea=1,
                              #params$nSp=params$nSp,
                              distNum="fix", 
                              paramNum=0.5){

    if(tolower(distNum)=="fix"){

        vectorX[sample(length(vectorX), numSpArea)]  <- 1

    }else{

        numSpArea  <- c(1 + as.integer(rexp(1,paramNum))) ## como mínimo 1, tambien puede cambiarse
        
        if( numSpArea > params$nSp ){
			numSpArea <- params$nSp  
			}

        ##cat("\n**\tnumSpArea",numSpArea,"\n") ## para revisar num sp
        
        vectorX[sample(length(vectorX), numSpArea)]  <- 1

    } 
        
        ## estos los usare mas adelante para simular distribuciones espaciales heterogeneas, no les gaste gasolina es para tenerlas en mente

#        spID[spID > numSpArea ]  <-  numSpArea
#        spID[spID <= 0 ]  <-  1
#        vectorX[spID]  <- 1

if (sum(vectorX) == 0){stop("La cagamos")}
    
    return(vectorX)
}

