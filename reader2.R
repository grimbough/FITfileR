readFile_withScaffold <- function(fileName, scaffold, message_defs) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- readHeader(con)
  
  plmt <- "-1";
  prev_lmt <- "0"
  defs_count <- list()
  pseudoMessageTab <- NULL
  
  while(seek(con, where = NA) < (file_header$data_size + 14)) {
    
    record_header <- readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      
      if(lmt == prev_lmt) {
        plmt <- as.character(as.integer(plmt) + 1)
      } else {
        plmt <- lmt
      }
      pseudoMessageTab <- rbind(pseudoMessageTab, c(lmt, plmt))
      prev_lmt <- lmt
      
      ## read the message definition just to get through the bytes
      message <- readMessage.definition(con, devFields = record_header$developer_data)
      
      defs_count[[ plmt ]] <- 1

    } else if(record_header$message_type == "data") {
      
      defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
      message <- readMessage.data(con, message_defs[[ defIdx ]])
      currentRow <- defs_count[[ defIdx ]]
      scaffold[[ defIdx ]][ currentRow , ] <- message$message
      defs_count[[ defIdx ]] <- defs_count[[ defIdx ]] + 1

    } else {
      stop("unknown message type")
    }
  }

  return(scaffold)
}

renameWithGlobalNumbers <- function(scaffold, defs) {
  
  globalMessageNum <- sapply(defs, function(x) { x$global_message_num } )
  
  result <- vector("list", length = length(unique(globalMessageNum)))
  
  for(i in seq_along(result)) {
    gmn <- unique(globalMessageNum)[i]
    idx <- which(globalMessageNum == gmn)
    result[[ i ]] <- bind_rows(scaffold[idx])
    value_name <- filter(key_value.global_message, key == gmn) %>% 
      select(value) %>% 
      as.character()
    names(result)[i] <- value_name
  }
  return(result)
}

processMessageType <- function(obj, name) {
  
  obj = jo
  name = "record"
  
  kv.table <- eval(parse(text = paste0("key_value.", name)))
  current <- obj[[ name ]]
  idx <- match(names(current), kv.table[['key']])
  names(current) <- kv.table[['value']][idx]

  obj[[ name ]] <- current
  return(obj)
  
}