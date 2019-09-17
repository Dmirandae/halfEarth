
conteo    <- function (matY) {
	
	## control DRME hist(apply(matY,1, sum))
	count0 <- apply(matY,1, sum)
    cSp <- sum(as.logical(count0))
    mSp <- mean(count0)
    
    count0 <- apply(matY,2, sum)
    cAr <- sum(as.logical(count0))
    mAr <- mean(count0)
         
         vRes <- c(cSp,mSp,cAr,mAr)
         names(vRes) <- c("cSp","mSp","cAr","mAr")

  return(vRes)
}


