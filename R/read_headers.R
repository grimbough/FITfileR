
## Reads the fit file header.  This should now be 14 bytes (but may be 12).
## Currently only data_size is really used, to determine how large the file is.
## 
.readFileHeader <- function(con) {
  
  header <- list()
  header$size <- readBin(con = con, what = "int", n = 1, size = 1)
  header$protocol_version <- readBin(con = con, what = "int", n = 1, size = 1)
  header$profile_version <- readBin(con = con, what = "int", n = 1, size = 2, endian = "little")
  header$data_size <- readBin(con = con, what = "int", n = 1, size = 4, endian = "little")
  header$data_type <- rawToChar(readBin(con = con, what = "raw", n = 4, size = 1))
  if(header$size == 14) {
    header$crc <- readBin(con = con, what = "int", n = 2, size = 1)
  }
  return(header)
}

## reads the 1 byte header that proceeds each record/message
## The 8th bit determines if this is a standard or compressed header and 
## we dispatch the appropriate function here
## 
.readRecordHeader <- function(con, prev_header) {
  
  header <- readBin(con = con, what = "raw", n = 1, size = 1)
  
  if(!is.null(prev_header) && identical(prev_header, header)) {
    header <- prev_header
  } 
  
  return(header)
  
}

isCompressed <- function(header) {
  header <- rawToBits(header)
  return(as.logical(header)[8])
}

isDefinition <- function(header) {
  as.logical(
    rawToBits(header)[7] 
  )
}

hasDeveloperData <- function(object) {
  
  if(is(object, "FitDefinitionMessage")) {
    header <- object@header
  } else if (is(object, "FitDataMessage")) {
    header <- object@definition@header
  } else {
    header <- object
  }
  
  as.logical(
    rawToBits(header)[6] 
  )
}

timeOffset <- function(header) {

  if(isCompressed(header)) {
    offset <- .binaryToInt(rawToBits(header)[1:5])
  } else {
    offset <- 0
  }
  return(offset)
}

localMessageNumber_header <- function(object) {
    
    header <- object

    if(isCompressed(header)) {
        rawToBits(header)[6:7] |>
            .binaryToInt()
    } else {
        rawToBits(header)[1:4] |>
            .binaryToInt()
    }
}

localMessageNumber <- function(object) {
    
    if(is(object, "FitDefinitionMessage")) {
        header <- object@header
    } else if (is(object, "FitDataMessage")) {
        header <- object@definition@header
    } else {
        header <- object
    }
    
    if(isCompressed(header)) {
        rawToBits(header)[6:7] |>
            .binaryToInt()
    } else {
        rawToBits(header)[1:4] |>
            .binaryToInt()
    }
}