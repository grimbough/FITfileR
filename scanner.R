source("~/projects/fitR/reader.R")

scanMessage.data <- function(con, definition) {
  
  fieldTypes <- definition$field_definition$base_type
  
  bytesRead <- 0
  
  message <- list()
  for(i in 1:length(fieldTypes)) {
    readInfo <- data_type_lookup[[ fieldTypes[i] ]]
    bytesRead <- bytesRead + (as.integer(readInfo[[3]]) * as.integer(readInfo[[4]]))
  }
  seek(con, where = bytesRead, origin = "current")
  return(NULL)
}


scanFile <- function(fileName) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- readHeader(con)
  
  plmt <- 0;
  prev_lmt <- 0
  defs <- list()
  
  ## read some records
  for(j in 1:2000) {
    #while(bytesRead < file_header$data_size) {
    
    record_header <- readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      
      if(lmt == prev_lmt) {
        plmt = plmt + 1
      } else {
        plmt <- lmt
      }
      prev_lmt <- lmt
      
      message <- readMessage.definition(con, devFields = record_header$developer_data)
      defs[[ plmt ]] <- message$message
      
      cat(lmt, "\t", plmt, "\t", defs[[ plmt ]]$global_message_num, "\n")
      
    } else if(record_header$message_type == "data") {
      
      message <- scanMessage.data(con, defs[[ plmt ]])
      
    } else {
      stop("unknown message type")
    }
    bytesRead <- bytesRead + message$bytesRead + 1
  }
  
  return(data)
}
