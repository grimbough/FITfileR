library(dplyr)
library(ggplot2)

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
    mutate(base_type = format(as.hexmode(base_type), width = 2))
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
  bytesRead <- 5 + (3*message$n_fields)
  if(devFields){
    ## do something with the developer fields
  }
  return(list(message = message,
              bytesRead = bytesRead))
}

data_type_lookup <- list(
  '00' = c('raw', FALSE, 1L, 1),
  '01' = c('integer', TRUE, 1L, 1),
  '02' = c('integer', FALSE, 1L, 1),
  '83' = c('integer', TRUE, 2L, 1),
  '84' = c('integer', FALSE, 2L, 1),
  '85' = c('integer', TRUE, 4L, 1),
  '86' = c('raw', TRUE, 1L, 4), ## this hould be unsigned int32
  '07' = c('character', TRUE, 1L, 1),
  '88' = c('numeric', TRUE, 4L, 1),
  '89' = c('numeric', TRUE, 8L, 1),
  '0a' = c('integer', FALSE, 1L, 1),
  '8b' = c('integer', FALSE, 2L, 1),
  '8c' = c('raw', TRUE, 1L, 4)  ## this hould be unsigned int32
)

readMessage.data <- function(con, definition) {
  
  fieldTypes <- definition$field_definition$base_type
  
  bytesRead <- 0
  
  message <- list()
  for(i in 1:length(fieldTypes)) {
    readInfo <- data_type_lookup[[ fieldTypes[i] ]]
    message[[i]] <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                            size = readInfo[[3]], n = readInfo[[4]], 
                            endian = definition$architecture)
    bytesRead <- bytesRead + (as.integer(readInfo[[3]]) * as.integer(readInfo[[4]]))
    if(is.character(message[[i]])) {
      bytesRead <- bytesRead + nchar(message[[i]])
    }
    if(fieldTypes[i] %in% c('86', '8c')) {
      if(definition$architecture == "little" )
        message[[i]] <- sum(2^(.subset(0:31, as.logical(rawToBits(message[[i]])))))
      else 
        message[[i]] <- sum(2^(.subset(0:31, as.logical(rawToBits(message[[i]][4:1])))))
    }
  }
  names(message) <- definition$field_definition$field_def_num
  message <- as_data_frame(message)
  return(list(message = message,
              bytesRead = bytesRead))
}

readFile <- function(fileName) {
  
  defs <- vector("list", 20)
  data <- list()
  data_counter <- list()
  messageTable <- data_frame(lmt = character(0), gmn = character(0))
  messages <- matrix(nrow = 2000, ncol = 5)
  bytesRead <- 0
  plmt = 0 ## pseudo local message type
  prev_lmt <- NULL
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- readHeader(con)
  
  ## read some records
  
  for(j in 1:2000) {
  #while(bytesRead < file_header$data_size) {
    
    record_header <- readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      
      plmt = plmt + 1
      
      message <- readMessage.definition(con, devFields = record_header$developer_data)
      defs[[ lmt ]] <- message$message

  #    cat(message$message$global_message_num, "\t", message$message$n_fields, "\n")
      data[[ as.character(plmt) ]] <- vector("list", 2000)
      data_counter[[ as.character(plmt) ]] <- 1
      
    } else if(record_header$message_type == "data") {
      
      message <- readMessage.data(con, defs[[ lmt ]])
      data[[ as.character(plmt) ]][[ data_counter[[ as.character(plmt) ]] ]] <- message$message
      data_counter[[ as.character(plmt) ]] <- data_counter[[ as.character(plmt) ]]+1
      
    } else {
      stop("unknown message type")
    }
    bytesRead <- bytesRead + message$bytesRead + 1
  }

  return(data)
}

readFile.garmin <- function(fileName) {
  
  defs <- list()
  data <- list()
  messageTable <- data_frame(lmt = character(0), gmn = character(0))
  bytesRead <- 0
  i = 0
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- readHeader(con)
  
  ## read some records
  #for(i in 1:2000) {
  while(bytesRead < file_header$data_size) {
    
    record_header <- readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      
      message <- readMessage.definition(con, devFields = record_header$developer_data)
      defs[[ lmt ]] <- message$message
      
      gmn <- as.character(defs[[ lmt ]]$global_message_num)
      messageTable <- rbind(messageTable, data_frame(lmt = lmt, gmn = gmn))
      
 #     cat(message$message$global_message_num, "\t", message$message$n_fields, "\n")
      
      i = i + 1
      data[[ lmt ]] <- NULL
      
    } else if(record_header$message_type == "data") {
      
      message <- readMessage.data(con, defs[[ lmt ]])
      data[[ lmt ]] <- rbind(data[[ lmt ]], message$message)
      
    } else {
      stop("unknown message type")
    }
    bytesRead <- bytesRead + message$bytesRead + 1
  #  cat(i, "\t", bytesRead, "\n")
  }
  
  ## for now, lets just return the 'record' table
  #lmt_record <- (filter(messageTable, gmn == 20) %>% 
  #  select(lmt))[[1]]
  
  return(data)
}

formatData <- function(dat) {
  dat <- dat %>%
    mutate(time = as.POSIXct(`253`, origin = "1989-12-31")) %>%
    rename(latitude = `0`, longitude = `1`, distance = `5`, altitude = `2`, speed = `6`, temperature = `13`)
  return(dat)
}


