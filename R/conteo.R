
conteo    <- function (matY) {
	
    cSp <- sum(as.logical(apply(matY,1, sum)))
    mSp <- mean(apply(matY,1, sum))

    cAr <- sum(as.logical(apply(matY,2, sum)))
    mAr <- mean(apply(matY,2, sum))
         
         vRes <- c(cSp,mSp,cAr,mAr)
         names(vRes) <- c("cSp","mSp","cAr","mAr")

  return(vRes)
}


