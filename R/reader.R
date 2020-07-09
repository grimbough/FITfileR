  
#' @export
readFitFile <- function(fileName, dropUnknown = TRUE, mergeMessages = TRUE) {
  
  data("data_type_lookup", package = "fitFileR", envir = parent.frame())
  
  tmp <- .readFile(fileName)
  all_records <- .renameMessages(tmp[[1]], tmp[[2]], merge = mergeMessages)
  
  for(i in names(all_records)) {
    all_records[[i]] <- .processMessageType(all_records[[i]], name = i, drop = dropUnknown)
  }
  
  fitFile <- new("FitFile", 
                 header = tmp[[3]],
                 file_id = all_records$file_id, 
                 events = all_records$event, 
                 records = all_records$record, 
                 laps = all_records$lap)
  
  return(fitFile)
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

        if(record_header$message_type == "definition") {
            
          #message("Def: ", lmt)
          
            if(lmt %in% pseudoMessageTab[,2]) {
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
          
          #message("Data: ", lmt)
            
            if(record_header$type == "compressed_timestamp") {
             # message("Compressed")
              defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
              message <- .readMessage.data(con, message_defs[[ defIdx ]], compressed_timestamp = TRUE)$message
              scaffold[[ defIdx ]] <- rbind(scaffold[[ defIdx ]], 
                                            message)
            } else {
          
              defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
              message <- .readMessage.data(con, message_defs[[ defIdx ]], compressed_timestamp = FALSE)
              scaffold[[ defIdx ]] <- dplyr::bind_rows(scaffold[[ defIdx ]], 
                                            message) 
            }
        } else {
            stop("unknown message type")
        }
    }
    
    if(length(message_defs) != length(scaffold)) {
        stop("Unequal lengths")
    } 
    
    scaffold <- lapply(scaffold, as_tibble)
    
    return(list(scaffold, message_defs, file_header))
}

