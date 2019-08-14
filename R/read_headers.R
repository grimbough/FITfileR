
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

## reads the 1 byte header that preceeds each record/message
## The 8th bit determines if this is a standard or compressed header and 
## we dispatch the appropriate function here
## 
## Currenly only normal headers are supported
## 
.readRecordHeader <- function(con) {
  
  record_header <- rawToBits(readBin(con = con, what = "raw", n = 1, size = 1))
  if(record_header[8]) {
    ## compressed time stamp header
    ## currently not handled
    # stop("Compressed time stamp header not currently supported")
    header <- .readMessageHeader_compressed(record_header)
  } else {
    ## normal header
    header <- .readMessageHeader_normal(record_header)
  }
  
}

## takes the 8 bits from a normal message header
## determines if this is a definition or data message, the presence of
## developer data, and the local message type.
## 
## Currently developer data isn't supported
##
.readMessageHeader_normal <- function(record_header) {
  
  header <- list()
  header$type <- "normal"
  header$message_type <- ifelse(record_header[7], "definition", "data")
  header$developer_data <- as.logical(record_header[6])
  header$local_message_type <- .binaryToInt(record_header[1:4])
  return(header)
  
}

## takes the 8 bits from a compressed timestamp message header
## determines the local message type and the time offset
##
.readMessageHeader_compressed <- function(record_header) {
  
  header <- list()
  header$type <- "compressed_timestamp"
  header$message_type <- "data"
  header$developer_data <- FALSE
  header$local_message_type <- .binaryToInt(record_header[6:7])
  header$time_offset <- .binaryToInt(record_header[1:5])
  return(header)
  
}