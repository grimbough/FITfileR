library(dplyr)

readHeader <- function(con) {
  
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

readRecordHeader <- function(con) {
  
  record_header <- rawToBits(readBin(con = con, what = "raw", n = 1, size = 1))
  if(record_header[8]) {
    ## compressed time stamp header
  } else {
    ## normal header
    header <- readNormalHeader(record_header)
  }
  
}

binaryToInt <- function(bits) {
  
  as.integer(sum(as.integer(bits) * 2^(seq_along(bits)-1)))
  
}

readNormalHeader <- function(record_header) {
  
  header <- list()
  header$type <- "normal"
  header$message_type <- ifelse(record_header[7], "definition", "data")
  header$developer_data <- as.logical(record_header[6])
  ## might need to change the order here, not sure on endianess
  header$local_message_type <- binaryToInt(record_header[1:4])
  return(header)
  
}

processFieldDefs <- function(fields) {
  
  fields <- as.integer(fields)
  fields <- split(fields, rep(1:3, by = length(fields)/3))
  names(fields) = c('field_def_num', 'size', 'base_type')
  fields <- as_data_frame(fields) %>%
    mutate(base_type = as.character(as.hexmode(base_type)))
  return(fields)
}

readMessage.definition <- function(con, devFields = FALSE) {
  
  message <- list()
  message$reserved <- readBin(con = con, what = "raw", n = 1, size = 1)
  message$architecture <- ifelse(readBin(con = con, what = "int", n = 1, size = 1),
                                 "big", "little")
  message$global_message_num <- readBin(con = con, what = "int", n = 1, size = 2,
                                       endian = message$architecture)
  message$n_fields <- readBin(con = con, what = "int", n = 1, size = 1)
  message$field_definition <- processFieldDefs(
      readBin(con = con, what = "raw", n = 3 * message$n_fields, size = 1)
    )
  if(devFields){
    ## do something with the developer fields
  }
  return(message)
}

data_type_lookup <- list(
  '00' = c('raw', FALSE, 1, 1),
  '01' = c('integer', TRUE, 1, 1),
  '02' = c('integer', FALSE, 1, 1),
  '83' = c('integer', TRUE, 2, 1),
  '84' = c('integer', FALSE, 2, 1),
  '85' = c('integer', TRUE, 4, 1),
  '86' = c('raw', TRUE, 1, 4), ## this hould be unsigned int32
  '07' = c('character', TRUE, 1, 1),
  '88' = c('numeric', TRUE, 4, 1),
  '89' = c('numeric', TRUE, 8, 1),
  '0a' = c('integer', FALSE, 1, 1),
  '8b' = c('integer', FALSE, 2, 1),
  '8c' = c('raw', TRUE, 1, 4)  ## this hould be unsigned int32
)

readMessage.data <- function(con, definition) {
  
  fieldTypes <- as.character(as.hexmode(definition$field_definition$base_type))
  
  message <- list()
  for(i in 1:length(fieldTypes)) {
    readInfo <- data_type_lookup[[ fieldTypes[i] ]]
    message[[i]] <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                            size = readInfo[[3]], n = readInfo[[4]])
    if(fieldTypes[i] %in% c('86', '8c')) {
      message[[i]] <- sum(2^(.subset(0:31, as.logical(rawToBits(message[[i]])))))
    }
  }
  names(message) <- definition$field_definition$field_def_num
  message <- as_data_frame(message)
  return(message)
}

readFile <- function() {
  
  defs <- list()
  data <- list()
  
  con <- file("/home/msmith/Code/Fit_files/fitR/2016-10-23-14-09-05.fit", "rb")
  on.exit(close(con))
  file_header <- readHeader(con)
  
  ## read some records
  for(i in 1:500) {
  #for(i in 1:file_header$data_size) {
    
    record_header <- readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      defs[[ lmt ]] <- 
        readMessage.definition(con, devFields = record_header$developer_data)
      data[[ lmt ]] <- NULL
    } else if(record_header$message_type == "data") {
      data[[ lmt ]] <-
        rbind(data[[ lmt ]], 
              readMessage.data(con, defs[[ lmt ]] ))
    } else {
      stop("unknown message type")
    }
    
  }
  return(list(defs, data))
}

# con <- file("/home/msmith/Code/Fit_files/fitR/2016-10-23-14-09-05.fit", "rb")
# file_header <- readHeader(con)
# record_header <- readRecordHeader(con)
# defintion_message <- readMessage.definition(con, devFields = record_header$developer_data)
# record_header2 <- readRecordHeader(con)
# data_message <- readMessage.data(con, defintion_message)
# record_header3 <- readRecordHeader(con)
# defintion_message2 <- readMessage.definition(con, devFields = record_header$developer_data)
# 
# close(con)
