
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

## https://www.r-bloggers.com/passing-arguments-to-an-r-script-from-command-lines/

    args <- commandArgs(trailingOnly = TRUE)

    if (length(args)==0){
        cat("Uso Rscript x.R parámetros en secuencia:\n\t",
            names(params),
            "\n")
        quit(save="no")
    }       

    if(length(args) != 0){args <- as.integer(as.character(args))
		}else{
			args <- as.integer(args[1:7])
			}

params$nSp             <-   args[1]

params$nCells          <-   args[2]

params$SpInCell        <-   args[3]

params$nReplicas       <-   args[4]

params$nBorrados       <-   args[5]

#if(params$nBorrados <= 0){params$nBorrados <- 1}

params$distribRichness <-   as.character(args[6])

params$nRate           <-   args[7]/100

}


## print(unlist(params))


## listado nSp NReplicas 
listado  <- rep(params$nSp,params$nReplicas)

#listado

## un listado de tablas limpias
tablasLimpias  <- lapply(listado,matrizLimpia,numCells=params$nCells) 

#tablasLimpias

asignadasIniciales  <- lapply(tablasLimpias, FUN=tmp1) 

#asignadasIniciales
#cat("\n")

MatrizIniciales  <- matrix(unlist(lapply(asignadasIniciales, FUN=conteo)),
                           ncol=4,byrow=T)

colnames(MatrizIniciales) <- c("cSp","mSp","cAr","mAr")
                                        #
#MatrizIniciales


#system.time(

if(params$nBorrados > 0){

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

finalDF <-  as.data.frame(cbind(salida0,salida1))

print(finalDF, row.names = FALSE)

}else{
	MatrizIniciales
	}



