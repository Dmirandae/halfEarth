

############################

rm(list=ls())

## funciones

a <- read.csv("source.R",header=F)

for( i in a$V1 ){ 
	source(i) 
	}


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


cat("param1: nSp",nSp,"Celdas",nCells,"replic",nReplicas)


## una sola de ejemplo

#matriz0  <-  matrizLimpia(nSp=nSp, nCells=nCells) 


## listado nSp NReplicas 
listado  <- rep(nSp,nReplicas)


## un listado de tablas limpias
tablasLimpias  <- lapply(listado,matrizLimpia,nCells=nCells) 

tmp1  <-  function(x){
    matDatAsig <- apply(x,
                        2,
                        asignarSpCeldas,
                        paramNum=nRate,
                        distNum=distribRichness,
                        numSpEnArea = SpEnArea)
    
    return(matDatAsig)
    }


asignadas  <- lapply(tablasLimpias, FUN=tmp1) 


                                        #print(asignadas)

                                        #
cat("\n")


datosIniciales <-matrix(
    unlist(lapply(asignadas, FUN=conteo)),
    ncol=4,byrow=T)

## aqui es solo una vez por matriz generada, debe hacerse n veces

borradas  <- lapply(asignadas, FUN=eliminarSpCeldas)


datosFinales <-   matrix(
    unlist(lapply(borradas, FUN=conteo)),
    ncol=4,byrow=T)

verdaderosResultados  <- cbind(
    as.data.frame(datosIniciales),
    as.data.frame(datosFinales))

names(verdaderosResultados) <-   c(paste0("Inicial_",c("cSp","mSp","cAr","mAr")),paste0("PostDel_",c("cSp","mSp","cAr","mAr")))


summary(verdaderosResultados)
