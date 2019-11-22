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

#if (1 != 1) {

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
            args <- as.integer(args[1:8])
            }

params$nSp             <-   args[1]

params$nCells          <-   args[2]

params$SpInCell        <-   args[3]

if(params$SpInCell > params$nSp){
	params$SpInCell <- params$nSp
	}

params$nReplicas       <-   args[4]

params$propBorra       <-   args[5]/100

if(params$propBorra > 100){
	params$propBorra <- 100
	}

params$distribRichness <-   as.character(args[6])

params$nRate           <-   args[7]/100

params$nRepeticiones   <-   args[8]

}


## print(unlist(params))


## listado nSp NReplicas
listado  <- rep(params$nSp,params$nRepeticiones)

#listado

#params

## un listado de tablas limpias
tablasLimpias  <- lapply(listado,crearMatrizLimpia,
                          numCells=params$nCells)

                                        #
#length(tablasLimpias)
#head(tablasLimpias[[1]])

asignadasIniciales  <- lapply(tablasLimpias, FUN=crearMatrizDatos)

                                        #

                                        #
#length(asignadasIniciales)
#head(asignadasIniciales[[1]])
#head(asignadasIniciales[[2]])
#head(asignadasIniciales[[3]])

                                        #cat("\n")

MatrizIniciales  <- matrix(unlist(lapply(asignadasIniciales, FUN=conteo)),
                           ncol=4,byrow=T)


colnames(MatrizIniciales) <- c("cSp","mSp","cAr","mAr")
                                        #
                                        #
#MatrizIniciales


#system.time(

if(params$propBorra != 0.0){

    asignadasBorradas <- lapply(asignadasIniciales,
                             FUN=perturbarMatriz,
                             nTimes=params$nReplicas,
                             probThanos=params$propBorra)   #

                                        #
#asignadasBorradas[[1]]
#asignadasBorradas[[2]]

salida  <- do.call(rbind, asignadasBorradas)

#salida

colnames(salida)  <-  c("cSp","mSp","cAr","mAr",
                        "borrados_cSp","borrados_mSp"," borrados_cAr"," borrados_mAr")

                                        #
#    salida



salida0  <- as.data.frame(salida)

#    salida0

    cont  <- length(salida0$cSp)

salida1  <- matrix(unlist(params),ncol=8,nrow=cont,byrow=TRUE)

colnames(salida1)  <-  c(names(params))

#    salida1
salida1  <-   as.data.frame(salida1)


## to set accordingly

options("width"=300)

finalDF <-  as.data.frame(cbind(salida0,salida1))

print(finalDF, row.names = FALSE)

}else{
    MatrizIniciales
    }
