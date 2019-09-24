
makeLatLong  <- function(Lat=10, Long=5){

    vectorN  <- 1:Long

    putX  <-  function(x){paste(x, 1:Lat)}

    return(as.vector(sapply(vectorN,putX)))
}
