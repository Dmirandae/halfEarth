
## todo : Roxygen!!!!!

############################

rm(list=ls())

## funciones

a <- read.csv("source.R",header=F)

for( i in a$V1 ){ 
	source(i) 
	}


##library(parallel)
 
# Calculate the number of cores
#no_cores <- detectCores() - 1
 
# Initiate cluster
                                        #
#cl <- makeCluster(no_cores)

##ls()

## parametros

	source("parametros.R")


if (!interactive()) {
       
    args <- as.integer(commandArgs(trailingOnly = TRUE))

    if (length(args)==0){
        cat("Uso Rscript x.R parámetros:\n
             nSp nCells nReplicas nBorrados\nBye\n")
        quit(save="no")
    }

    nSp        <-  args[1]
    nCells     <-  args[2]
    nReplicas  <-  args[3]
    nBorrados  <-  args[4]

}

## cat("param1: nSp",nSp,"Celdas",nCells,"replic",nReplicas)


## una sola de ejemplo

#matriz0  <-  matrizLimpia(nSp=nSp, nCells=nCells) 


## listado nSp NReplicas 
listado  <- rep(nSp,nReplicas)


## un listado de tablas limpias
tablasLimpias  <- lapply(listado,matrizLimpia,nCells=nCells) 

#tablasLimpias

tmp1  <-  function(x){
    matDatAsig <- apply(x,
                        2,
                        asignarSpCeldas,
                        paramNum=nRate,
                        distNum=distribRichness,
                        numSpEnArea = SpEnArea)
    
    return(matDatAsig)
    }


asignadasIniciales  <- lapply(tablasLimpias, FUN=tmp1) 


#cat("\n")


MatrizIniciales  <- matrix(unlist(lapply(asignadasIniciales, FUN=conteo)),
                           ncol=4,byrow=T)

colnames(MatrizIniciales) <- c("cSp","mSp","cAr","mAr")

#MatrizIniciales


tmp2  <-  function(x,nTimes){

    timesThanos  <-  list()

    for(numTemp0 in 1:nTimes){

        timesThanos[[numTemp0]]  <- x
    }

     borradas  <- lapply(timesThanos, FUN=eliminarSpCeldas)
    
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
    

#system.time(

asignadasBorradas  <- lapply(asignadasIniciales, FUN=tmp2, nTimes=nBorrados)   #)


#asignadasBorradas 

salida  <- matrix(unlist(asignadasBorradas),ncol=8,byrow=TRUE)

colnames(salida)  <-  c("cSp","mSp","cAr","mAr","borrados_cSp","borrados_mSp"," borrados_cAr"," borrados_mAr")


as.data.frame(salida)

