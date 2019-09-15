
## todo : Roxygen!!!!!

############################

rm(list=ls())

## funciones

source("parametros.R")


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



if (!interactive()) {
       
    args <- as.integer(commandArgs(trailingOnly = TRUE))

    if (length(args)==0){
        cat("Uso Rscript x.R parámetros:\n
             nSp params$nCells params$nReplicas params$nBorrados\nBye\n")
        quit(save="no")
    }

    params$nSp        <-  args[1]
    params$nCells     <-  args[2]
    params$nReplicas  <-  args[3]
    params$nBorrados  <-  args[4]

}


## print(unlist(params))


## listado nSp NReplicas 
listado  <- rep(params$nSp,params$nReplicas)


## un listado de tablas limpias
tablasLimpias  <- lapply(listado,matrizLimpia,numCells=params$nCells) 

#tablasLimpias

tmp1  <-  function(x){
    matDatAsig <- apply(x,
                        2,
                        asignarSpCeldas,
                        paramNum=params$nRate,
                        distNum=params$distribRichness,
                        numSpArea = params$SpEnArea)
    
    return(matDatAsig)
    }


asignadasIniciales  <- lapply(tablasLimpias, FUN=tmp1) 

#asignadasIniciales
#cat("\n")


MatrizIniciales  <- matrix(unlist(lapply(asignadasIniciales, FUN=conteo)),
                           ncol=4,byrow=T)

colnames(MatrizIniciales) <- c("cSp","mSp","cAr","mAr")

                                        #
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

asignadasBorradas  <- lapply(asignadasIniciales, FUN=tmp2, nTimes=params$nBorrados)   #)

#asignadasBorradas 

salida  <- do.call(rbind, asignadasBorradas)


colnames(salida)  <-  c("cSp","mSp","cAr","mAr",
                        "borrados_cSp","borrados_mSp"," borrados_cAr"," borrados_mAr")

#salida

#tpt2  <- matrix(resInicial, ncol=4,nrow=nTimes,byrow=TRUE)
#tpt2  <-  as.data.frame(tpt1)


salida0  <- as.data.frame(salida)

cont  <- length(salida0$cSp)

salida1  <- matrix(unlist(params),ncol=7,nrow=cont,byrow=TRUE)

colnames(salida1)  <-  c(names(params))

salida1  <-   as.data.frame(salida1)


## to set accordingly

options("width"=300)

cbind(salida0,salida1)
