
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


##print(unlist(params))


## listado nSp NReplicas 
listado  <- rep(params$nSp,params$nReplicas)

                                        #
listado

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

head(MatrizIniciales)

dim(asignadasIniciales[[1]])

matrizLatLong  <- makeLatLong(10,10)

sp1  <- which(asignadasIniciales[[1]][1,]==1)

length(sp1)


MST.Propincuity(distancia)

distancia  <- dist(matrizLatLong[sp1,])

dist.Matrix  <- as.matrix(distancia)

  large.value <- max(dist.Matrix) + 1
  
  diag(dist.Matrix) <- large.value
  
  minimos <- apply(dist.Matrix,1,min)
  
  parejas.minimos <- matrix(NA,nrow =length(minimos),ncol = 2 )
  
  for (i in 1:length(minimos)){
    parejas.minimos[i,1] <- i
    parejas.minimos[i,2] <- which(dist.Matrix[,i]==minimos[i])
  }
  
  eliminar  <- NULL
  
  for (i in 1:(length(parejas.minimos[,1])-1)){
    for (j in (i+1):length(parejas.minimos[,1])){
      if (all(parejas.minimos[i,] %in% parejas.minimos[j,])){
#        print(paste("a eliminar en ",i,"---",j))
        parejas.minimos[j,] <-c(-rnorm(1),-rnorm(1))
        eliminar <- c(eliminar,j) 
      }
    }
  }
  
  minimos <- minimos[-eliminar]

##Propincuity.2(distancia)
