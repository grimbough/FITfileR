.binaryToInt <- function(bits) {
  int <- as.integer(sum(as.integer(bits) * 2^(seq_along(bits)-1)))
  return(int)
}

## convert double representing a uint32 into a raw vector
.uintToBits <- function(x) {
    
    if(x >= 0 && x < 2^31) {
        res <- intToBits(x)
    } else if(x >= 2^31 && x < 2^32) {
        x <- x-(2^31)
        res <- intToBits(x)
        res[32] <- as.raw(1)
    } else {
        stop("Unable to convert ", x, " to unit32. Outside range.")
    }
    return(res)
}