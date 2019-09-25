
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

params$SpEnArea        <-   args[3]

params$nReplicas       <-   args[4]

params$nBorrados       <-   args[5]

#if(params$nBorrados <= 0){params$nBorrados <- 1}

params$distribRichness <-   as.character(args[6])

params$nRate           <-   args[7]/100

}


##print(unlist(params))


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


makeLatLong(5,10)

