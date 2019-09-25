
makeLatLong  <- function(Lat=10, Long=5){

    vectorN  <- 1:Long

    putX  <-  function(x){paste(x, 1:Lat)}

    tmp0  <- as.vector(sapply(vectorN,putX))

    tmp0  <- matrix(as.integer(unlist(strsplit(tmp0," "))),ncol = 2, byrow = T)

    return(tmp0)

}
