## Given the populated list of data, and the list of message definitions,
## looks up the plain text name of each message type associated with the
## global message number. Drops any that can't be identified.
## Also merges entries with the same message number but different defintions.
.renameMessages <- function(scaffold, defs, merge = TRUE) {
  
  ## load the appropriate key/value table
  data("fit_data_types", 
       package = "fitFileR", 
       envir = environment())
  
  globalMessageNum <- sapply(defs, function(x) { x$global_message_num } )
  
  ## we are going to remove any entries that have a global message number
  ## we can't identify from the SDK.  (Edge 500 has message 22 a lot).
  rm_idx <- which(!globalMessageNum %in% fit_data_types$mesg_num[['key']])
  if(length(rm_idx)) {
    scaffold <- scaffold[ -rm_idx ]
    globalMessageNum <- globalMessageNum[ -rm_idx ]
  }
  
  if(merge) {
  ## Merge entries with the same global message number, but are separate.
  ## This arises when a few records are written with different columns 
  ## e.g. before a cadence or hr sensor is detected. We NA missing values.
  result <- vector("list", length = length(unique(globalMessageNum)))
  for(i in seq_along(result)) {
    gmn <- unique(globalMessageNum)[i]
    idx <- which(globalMessageNum == gmn)
    result[[ i ]] <- bind_rows(scaffold[idx])
    value_name <- filter(fit_data_types$mesg_num, key == gmn) %>% 
      select(value) %>% 
      as.character()
    names(result)[i] <- value_name
  }
  return(result)
  } else {
    for(gmn in unique(globalMessageNum)) {
      idx <- which(globalMessageNum == gmn)
      value_names <- filter(fit_data_types$mesg_num, key == gmn) %>% 
        select(value) %>% 
        paste(seq_along(idx), sep = "-")
      names(scaffold)[idx] <- value_names
      }
  return(scaffold)
  }
}

.processMessageType <- function(obj, name, drop = TRUE) {
  
  ## load the appropriate key/value table
  data("fit_message_types", 
       package = "fitFileR", 
       envir = environment())
  
  ## strip any numbering from the message name
  name_short <- gsub(x = name, pattern = "-[0-9]*", replacement = "")
  
  if(!name_short %in% names(fit_message_types)) {
    warning("Renaming variables for message type '", name, "' is not currently supported")
  } else {
    
    message.table <- fit_message_types[[ name_short ]]
    current <- obj[[ name ]]
    
    idx <- match(names(current), message.table[['key']])
    if(any(is.na(idx))) {
      if(drop) {
        current <- current[,-which(is.na(idx))]
        idx <- idx[-which(is.na(idx))]
        names(current) <- message.table[['value']][idx]
      } else {
        names(current)[which(!is.na(idx))] <- message.table[['value']][idx[which(!is.na(idx))]]
      }
    } else { ## TODO:  come back and tidy this logic up later
      names(current)[which(!is.na(idx))] <- message.table[['value']][idx[which(!is.na(idx))]]
    }
    
    for(i in seq_along(current)) {
      current[[i]] <- .fixDataType(values = current[[i]],
                                   type = message.table[['type']][ idx[i] ])
    }
    
    ## some values need to be divided by a scaling factor
    scale_table <- filter(message.table, value %in% names(current), !is.na(scale))
    for(i in seq_len(nrow(scale_table))) {
       idx <- match(scale_table$value[i], names(current))
       current[[ idx ]] <- current[[ idx ]] / scale_table$scale[i]
    }
    
    current <- .fixGarminProducts(current)
    obj[[ name ]] <- current
  }

  return(obj)
}

## lots of fields store numeric references to a value in a factor
## this function takes the name of the appropriate data type
## and checks whether it is a time stamp, or we should test
## whether it comes from a factor data type.
.fixDataType <- function(values, type) {
  
  if(grepl(pattern = "date_time", x = type)) {
    values <- .adjustTimeStamp(values)
  } else {
    values <- .getFromDataTypeFactor(values, type)
  }
  return(values)
}

## fit time stamps are from 31st December 1989
## this function transforms them into data/times
.adjustTimeStamp <- function(values) {
  as.POSIXct(values, origin = "1989-12-31")  
}

.getFromDataTypeFactor <- function(values, type) {
  
  ## for 'non-standard' units, see if we have them stored and 
  ## replace if we can
  data("fit_data_types", 
       package = "fitFileR", 
       envir = environment())
  
  enum <- fit_data_types[[ as.character(type) ]]
  if(!is.null(enum) && is.integer(values)) {
    idx <- match(values, enum[['key']])
    values <- enum[['value']][ idx ]
  }
  return(values)
}

.fixGarminProducts <- function(message) {
  if(("manufacturer" %in% names(message)) && ("product" %in% names(message))) {
    garmin_idx <- which(message$manufacturer == "garmin")
    if(length(garmin_idx)) {
      message$product[garmin_idx] <- .getFromDataTypeFactor(values = message$product[garmin_idx], 
                                                            type = "garmin_product")
    }
  }
  return(message)
}

