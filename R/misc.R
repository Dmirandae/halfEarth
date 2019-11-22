crearMatrizDatos  <-  function(x){
    matDatAsig <- apply(x,
                        2,
                        asignarSpCeldas,
                        paramNum   = params$nRate,
                        distNum    = params$distribRichness,
                        numSpArea  = params$SpInCell)
    
    return(matDatAsig)
    }


######################3




perturbarMatriz  <-  function(x,nTimes,probThanos=params$propBorra){

## lista de matrices para perturar

    timesThanos  <-  list()

## n copias de la matriz inicial, tantas como replicas se hayan propuesto

    for(numTemp0 in 1:nTimes){

        timesThanos[[numTemp0]]  <- x
    }

## lista lista


## borrar de acuerdo a la probabilidad de borrado por area

     borradas  <- lapply(timesThanos, FUN=eliminarSpMatriz,probThanos=params$propBorra)
    
#    borradas  <- mclapply(timesThanos, eliminarSpCeldas,
#                          mc.cores = no_cores)
   
    
    datosFinales <- matrix(unlist(lapply(borradas, FUN=conteo)),
                           ncol=4,byrow=T)

    verdaderosResultadosborrados  <-  as.data.frame(datosFinales)

    names(verdaderosResultadosborrados) <- c(paste0("borrados_",
                                                    c("cSp","mSp","cAr","mAr")))

   resInicial  <- conteo(x)
                           
    tpt1  <- matrix(resInicial, ncol=4,nrow=nTimes,byrow=TRUE)

    tpt1  <-  as.data.frame(tpt1)
    
    colnames(tpt1) <- c("cSp","mSp","cAr","mAr")
    
    ver1  <-  cbind(as.data.frame(tpt1),
                    as.data.frame(verdaderosResultadosborrados))    
    
    return(ver1)
}
    

