## Given the populated list of data, and the list of message definitions,
## looks up the plain text name of each message type associated with the
## global message number. Drops any that can't be identified.
## Also merges entries with the same message number but different definitions.
#' @importFrom dplyr filter
#' @importFrom utils data
.renameMessages <- function(scaffold, defs, merge = TRUE) {
  
  globalMessageNum <- sapply(defs, function(x) { x$global_message_num } )
  
  ## we are going to remove any entries that have a global message number
  ## we can't identify from the SDK.  (Edge 500 has message 22 a lot).
  rm_idx <- which(!globalMessageNum %in% fit_data_types$mesg_num[['key']])
  if(length(rm_idx)) {
    scaffold <- scaffold[ -rm_idx ]
    defs <- defs[ -rm_idx ]
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
    
    ## find the subset of the message definitions
    defs_sub <- defs[idx]
    if(length(defs_sub) > 1) {
      longest_def <- vapply(defs_sub, function(x) x$n_fields, 
                          FUN.VALUE = integer(1), USE.NAMES = FALSE ) %>%
        which.max()
      defs_sub <- defs_sub[ longest_def ]
    } 
    
    message <- new("FitMessage", global_message_number = gmn, 
        field_definition = defs_sub[[1]]$field_definition,
        messages = bind_rows(scaffold[idx]))
    
    result[[ i ]] <- message
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
  #data("fit_message_types", 
  #     package = "fitFileR", 
  #     envir = environment())
  
  ## strip any numbering from the message name
  name_short <- gsub(x = name, pattern = "-[0-9]*", replacement = "")
  
  if(!name_short %in% names(fit_message_types)) {
    warning("Renaming variables for message type '", name, "' is not currently supported")
  } else {
    
    message_table <- fit_message_types[[ name_short ]]
    ##current <- obj[[ name ]]
    current <- obj
    
    idx <- match(names(current@messages), message_table[['key']])
    if(any(is.na(idx)) && isTRUE(drop)) {
      current@messages <- current@messages[,-which(is.na(idx))]
      idx <- idx[-which(is.na(idx))]
      names(current@messages) <- message_table[['value']][idx]
    } else {
      names(current@messages)[which(!is.na(idx))] <- message_table[['value']][idx[which(!is.na(idx))]]
    }
    
    #data("fit_data_types", 
    #     package = "fitFileR", 
    #     envir = environment())
    
    for(i in seq_along(current@messages)) {
      current@messages[[i]] <- .fixDataType(values = current@messages[[i]],
                                   type = message_table[['type']][ idx[i] ],
                                   fit_data_types)
    }
    
    ## some values need to be divided by a scaling factor
    scale_table <- filter(message_table, value %in% names(current@messages), !is.na(scale))
    for(i in seq_len(nrow(scale_table))) {
       idx <- match(scale_table$value[i], names(current))
       if(is.list(current[[ idx ]])) {
         current@messages[[ idx ]] <- lapply(current[[idx]], function(x, scale) { x / scale }, 
                                    scale = as.numeric(scale_table$scale[i]))
       } else {
         current@messages[[ idx ]] <- current[[ idx ]] / as.numeric(scale_table$scale[i])
       }
    }
    
    current@messages <- .fixGarminProducts(current@messages, fit_data_types)
    #obj[[ name ]] <- current
  }

  return(current)
}

## lots of fields store numeric references to a value in a factor
## this function takes the name of the appropriate data type
## and checks whether it is a time stamp, or we should test
## whether it comes from a factor data type.
.fixDataType <- function(values, type, fit_data_types) {
  
  if(grepl(pattern = "date_time", x = type)) {
    values <- .adjustTimeStamp(values)
  } else {
    values <- .getFromDataTypeFactor(values, type, fit_data_types)
  }
  return(values)
}

## fit time stamps are from 31st December 1989
## this function transforms them into data/times
.adjustTimeStamp <- function(values) {
  as.POSIXct(values, origin = "1989-12-31")  
}

.getFromDataTypeFactor <- function(values, type, fit_data_types) {
  
  ## for 'non-standard' units, see if we have them stored and 
  ## replace if we can
  # data("fit_data_types", 
  #      package = "fitFileR", 
  #      envir = environment())
  # 
  enum <- fit_data_types[[ as.character(type) ]]
  if(!is.null(enum) && is.integer(values)) {
    idx <- match(values, enum[['key']])
    values <- enum[['value']][ idx ]
  }
  return(values)
}

.fixGarminProducts <- function(message, fit_data_types) {
  if(("manufacturer" %in% names(message)) && ("product" %in% names(message))) {
    garmin_idx <- which(message$manufacturer == "garmin")
    if(length(garmin_idx)) {
      message$product[garmin_idx] <- .getFromDataTypeFactor(values = message$product[garmin_idx], 
                                                            type = "garmin_product",
                                                            fit_data_types)
    }
  }
  return(message)
}

