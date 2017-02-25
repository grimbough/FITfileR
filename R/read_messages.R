.processFieldDefs <- function(fields) {
  
  fields <- as.integer(fields)
  fields <- split(fields, rep(1:3, by = length(fields)/3))
  names(fields) = c('field_def_num', 'size', 'base_type')
  fields <- as_data_frame(fields) %>%
    mutate(base_type = format(as.hexmode(base_type), width = 2))
  return(fields)
}

.readMessage.definition <- function(con, devFields = FALSE) {
  
  message <- list()
  message$reserved <- readBin(con = con, what = "raw", n = 1, size = 1)
  message$architecture <- ifelse(readBin(con = con, what = "int", n = 1, size = 1),
                                 "big", "little")
  message$global_message_num <- readBin(con = con, what = "int", n = 1, size = 2,
                                        endian = message$architecture)
  message$n_fields <- readBin(con = con, what = "int", n = 1, size = 1)
  message$field_definition <- .processFieldDefs(
    readBin(con = con, what = "raw", n = 3 * message$n_fields, size = 1)
  )
  bytesRead <- 5 + (3*message$n_fields)
  if(devFields){
    ## do something with the developer fields
    ## currently not supported
  }
  return(list(message = message,
              bytesRead = bytesRead))
}


.readMessage.data <- function(con, definition) {
  
  fieldTypes <- definition$field_definition$base_type
  
  bytesRead <- 0
  
  message <- list()
  for(i in 1:length(fieldTypes)) {
    readInfo <- data_type_lookup[[ fieldTypes[i] ]]
    message[[i]] <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                            size = readInfo[[3]], n = readInfo[[4]], 
                            endian = definition$architecture)
    bytesRead <- bytesRead + (as.integer(readInfo[[3]]) * as.integer(readInfo[[4]]))
    #if(is.character(message[[i]])) {
    #  bytesRead <- bytesRead + nchar(message[[i]])
    #}
    
    ## if we have unsigned ints, turn the bits into a numeric
    if(fieldTypes[i] %in% c('86', '8c')) {
      if(definition$architecture == "little") {
        bits <- as.logical(rawToBits(message[[i]][1:4]))
      } else {
        bits <- as.logical(rawToBits(message[[i]][4:1]))
      }
      message[[i]] <- sum(2^(.subset(0:31, bits)))
    }
    
  }
  names(message) <- definition$field_definition$field_def_num
  message <- as_data_frame(message)
  return(list(message = message,
              bytesRead = bytesRead))
}

.readMessage.data2 <- function(con, definition, scaffold, defIdx, row) {
  
  fieldTypes <- definition$field_definition$base_type
  
  bytesRead <- 0
  
  #message <- list()
  for(i in 1:length(fieldTypes)) {
    readInfo <- data_type_lookup[[ fieldTypes[i] ]]
    message <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                            size = readInfo[[3]], n = readInfo[[4]], 
                            endian = definition$architecture)
    bytesRead <- bytesRead + (as.integer(readInfo[[3]]) * as.integer(readInfo[[4]]))
    if(is.character(message)) {
      bytesRead <- bytesRead + nchar(message)
    }
    
    ## if we have unsigned ints, turn the bits into a numeric
    if(fieldTypes[i] %in% c('86', '8c')) {
      if(definition$architecture == "little") {
        bits <- as.logical(rawToBits(message[1:4]))
      } else {
        bits <- as.logical(rawToBits(message[4:1]))
      }
      message <- sum(2^(.subset(0:31, bits)))
    }
    scaffold[[ defIdx ]][row,i] <- message
  }
  #names(message) <- definition$field_definition$field_def_num
  #message <- as_data_frame(message)
  return(scaffold)
}