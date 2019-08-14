  
#' @export
readFitFile <- function(fileName, dropUnknown = TRUE) {
  
  data("data_type_lookup", package = "fitFileR", envir = parent.frame())
  
  tmp <- .readFile(fileName)
  all_records <- .renameMessages(tmp[[1]], tmp[[2]])
  
  for(i in names(all_records)) {
    all_records <- .processMessageType(all_records, name = i, drop = dropUnknown)
  }
  
  return(all_records)
}

.readFile <- function(fileName) {
    
    con <- file(fileName, "rb")
    on.exit(close(con))
    file_header <- .readFileHeader(con)
    
    message_defs <- list()
    defs_idx <- 1
    
    plmt <- "-1"
    prev_lmt <- "0"
    defs_count <- list()
    pseudoMessageTab <- NULL
    
    scaffold <- list()
    
    while(seek(con, where = NA) < (file_header$data_size + 14)) {
        
        record_header <- .readRecordHeader(con)
        lmt <- as.character(record_header$local_message_type)
        message(lmt)
        if(record_header$message_type == "definition") {
            
            if(lmt == prev_lmt) {
                plmt <- as.character(as.integer(plmt) + 1)
            } else {
                plmt <- lmt
            }
            pseudoMessageTab <- rbind(pseudoMessageTab, c(lmt, plmt))
            prev_lmt <- lmt
            
            ## read the message definition just to get through the bytes
            message_res <- .readMessage.definition(con, devFields = record_header$developer_data)
            message_defs[[ plmt ]] <- message_res$message
            defs_idx <- defs_idx + 1
            
            defs_count[[ plmt ]] <- 1
            
        } else if(record_header$message_type == "data") {
            
            if(record_header$type == "compressed_timestamp") {
              defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
              message <- .readMessage.data(con, message_defs[[ defIdx ]], compressed_timestamp = TRUE)$message
              scaffold[[ defIdx ]] <- rbind(scaffold[[ defIdx ]], 
                                            message)
            } else {
          
              defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
              scaffold[[ defIdx ]] <- rbind(scaffold[[ defIdx ]], 
                                            .readMessage.data(con, message_defs[[ defIdx ]])$message)
            }
            
        } else {
            stop("unknown message type")
        }
    }
    
    if(length(message_defs) != length(scaffold)) {
        stop("Unequal lengths")
    } 
    
    scaffold <- lapply(scaffold, as_tibble)
    
    return(list(scaffold, message_defs))
}

