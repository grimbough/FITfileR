.binaryToInt <- function(bits) {
  int <- as.integer(sum(as.integer(bits) * 2^(seq_along(bits)-1)))
  return(int)
}