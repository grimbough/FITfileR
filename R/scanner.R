#' @import tibble
.scanMessage.data <- function(con, definition) {
  
  fieldTypes <- definition$field_definition$base_type
  
  ##if we have character data we have to treat it a little differently
  ## since we don't know the length of the string.
  if('07' %in% fieldTypes) {
    totalBytesRead <- 0
    message <- list()
    for(i in 1:length(fieldTypes)) {
      readInfo <- data_type_lookup[[ fieldTypes[i] ]]
      if(readInfo[1] != "character") {
        bytesRead <- as.integer(readInfo[[3]]) * as.integer(readInfo[[4]])
        seek(con, where = bytesRead, origin = "current")
      } else {
        message <- readBin(con, what = "character", signed = readInfo[[2]],
                           size = readInfo[[3]], n = 1, 
                           endian = definition$architecture)
        bytesRead <- nchar(message)
      }
      totalBytesRead <- totalBytesRead + bytesRead 
    }
  ## if there aren't any strings we can just calc how many bytes we need
  } else {
    dataTable <- do.call("rbind", data_type_lookup[ fieldTypes ])
    bytesRead <- sum(as.integer(dataTable[,3]) * as.integer(dataTable[,4]))
    seek(con, where = bytesRead, origin = "current")
    totalBytesRead <- bytesRead
  }
  return(totalBytesRead)
}


.scanFile <- function(fileName) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  
  plmt <- "-1";
  pseudoMessageTab <- NULL
  prev_lmt <- "0"
  defs <- list()
  defs_count <- list()

  file_header <- .readFileHeader(con)
  
  ## read some records
  while(seek(con, where = NA) < (file_header$data_size + 14)) {
    
    record_header <- .readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {

      if(lmt == prev_lmt) {
        plmt <- as.character(as.integer(plmt) + 1)
      } else {
        plmt <- lmt
      }
      pseudoMessageTab <- rbind(pseudoMessageTab, c(lmt, plmt))
      prev_lmt <- lmt
      
      message <- .readMessage.definition(con, devFields = record_header$developer_data)
      defs[[ plmt ]] <- message$message
      defs_count[[ plmt ]] <- 0

    } else if(record_header$message_type == "data") {
      
      defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
      message <- .scanMessage.data(con, defs[[ defIdx ]])
      defs_count[[ defIdx ]] <- defs_count[[ defIdx ]] + 1
      
    } else {
      stop("unknown message type")
    }
  }
  
  return(list(defs, defs_count))
}

.buildMessageStructure <- function(defs, defs_count) {
  
  scaffold <- list()
    for(i in 1:length(defs)) {

      fieldTypes <- defs[[i]]$field_definition$base_type
      dataTypes <- do.call("rbind", data_type_lookup[ fieldTypes ])[,1]
      
      ## unsigned ints will be stored as numerics in our object
      if(any(names(dataTypes) %in% c('86', '8c'))) {
        idx <- which(names(dataTypes) %in% c('86', '8c'))
        dataTypes[idx] <- "numeric"
      }
      
      nMessages <- defs_count[[i]]
      
      structure <- sapply(paste0(dataTypes, "(", nMessages, ")"), 
                          function(x) eval(parse(text = x)),
                          simplify = FALSE, USE.NAMES = FALSE)
      structure <- as_data_frame(structure, validate = FALSE) %>%
        setNames(defs[[i]]$field_definition$field_def_num)
      scaffold[[ names(defs)[i] ]] <- structure
    }
  return(scaffold)
}


