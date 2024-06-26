## FIT time stamps are from 31st December 1989
## this function transforms them into date/times
.adjustTimeStamp <- function(values) {
  values <- as.integer(values)
  as.POSIXct(values, origin = "1989-12-31", tz = "UTC")  
}

.getFromDataTypeFactor <- function(values, type) {
  
  ## for 'non-standard' units, see if we have them stored and 
  ## replace if we can
  enum <- fit_data_types[[ as.character(type) ]]
  if(!is.null(enum) && is.integer(values)) {
    idx <- match(values, enum[['key']])
    values <- enum[['value']][ idx ]
  }
  return(values)
}


.fixGarminProducts <- function(message_table) {
  if(("manufacturer" %in% names(message_table)) && ("product" %in% names(message_table))) {
    garmin_idx <- which(message_table$manufacturer == "garmin")
    if(length(garmin_idx)) {
      message_table$product[garmin_idx] <- .getFromDataTypeFactor(values = as.integer(message_table$product[garmin_idx]), 
                                                            type = "garmin_product")
    }
  }
  return(message_table)
}

## convert message number into text version of its name
.translateGlobalMessageNumber <- function(global_message_number) {
  fit_data_types$mesg_num$value[which(fit_data_types$mesg_num$key == global_message_number)]
}

## convert message name into numeric form
.translateGlobalMessageName <- function(global_message_name) {
  fit_data_types$mesg_num$key[which(fit_data_types$mesg_num$value == global_message_name)] %>%
    as.integer()
}


## return details of a field number for a specific message type
.translateField <- function(field_definition_number, global_message_number) {
  
  global_message_name <- .translateGlobalMessageNumber(global_message_number)

  if(length(global_message_name) == 0) {
    return(list(value = '', key = '', type = '', units = NA))
  } else {
    type <- fit_message_types[[ global_message_name ]]
    type[which(type$key == field_definition_number), ]
  }
  
}


## only returns the field name
.translateField2 <- function(field_definition_number, global_message_number) {
  
  global_message_name <- .translateGlobalMessageNumber(global_message_number)
  type <- fit_message_types[[ global_message_name ]]
  type[which(type$key == field_definition_number), ]$value
  
}


.applyScaleAndOffset <- function(input, field_definition_number, global_message_number) {
  
  global_message_name <- .translateGlobalMessageNumber(global_message_number)
  if(length(global_message_name) == 0) { 
    details <- list(scale = NA, offset = NA, units = NA)
  } else {
    type <- fit_message_types[[ global_message_name ]]
    details <- type[which(type$key == field_definition_number), ]
  }
  
  ## divide by scaling factor if it exists
  if(!is.na(details$scale[1])) {
    if(is.list(input)) {
      input[[1]] <- as.numeric(input[[1]]) / as.numeric(details$scale[1])
    } else {
      input <- as.numeric(input) / as.numeric(details$scale[1])
    }
  }
  
  ## subtract offset if it exists
  if(!is.na(details$offset[1])) {
      input <- as.numeric(input) - details$offset[1]
  }
  
  ## add units as attribute
  if(!is.na(details$units[1])) {
      attributes(input) <- list(units = details$units)
  }
  
  return( input )
}

## expects a vector of 'input' values
.leftRightAdjustment <- function(input, type) {
  
  values <- lapply(as.integer(input), .uintToBits)
  
  if(type == "left_right_balance_100") {
    mask <- .uintToBits(16383)
    side_known <-  as.logical(values[[1]][16])
  } else {
    mask <- .uintToBits(127)
    side_known <-  as.logical(values[[1]][8])
  }
  
  output <- vapply(values, FUN = function(x,y) { 
    .binaryToInt(x & y) 
  }, mask, FUN.VALUE = integer(1))
  
  if(type == "left_right_balance_100")
    output <- output / 100
  
  attr(output, "units") <- "percentage"
  attr(output, "side") <- ifelse(side_known, 
                                yes = "right",
                                no = "unknown")
  output
}

.applyFormatConversions <- function(input, field_definition_number, global_message_number) {
  
  global_message_name <- .translateGlobalMessageNumber(global_message_number)
  type <- fit_message_types[[ global_message_name ]]
  details <- type[which(type$key == field_definition_number), ]
  
  type <- as.character(details$type)
  
  if(type == "date_time") {
    input <- .adjustTimeStamp(input)
    attr(input, "units") <- NULL
  } else if (!is.na(details$units) && details$units == "semicircles") {
    input <- as.integer(input) * (180 / 2^31)
    attributes(input) <- list(units = "degrees")
  } else if (grepl("left_right_balance", x = type)) {
    input <- .leftRightAdjustment(input, type)
  } else if(type %in% names(fit_data_types)) {
    ## here we catch all enum types.
    data_type <- fit_data_types[[ type ]]
    input <- data_type[ match(input, data_type$key),  ]$value
  }
  
  return(input)
}

#' Detect whether a given field number is defined for a specified global
#' message type.  Garmin (and maybe others) include many fields that are not
#' documented in the FIT Profile
#' 
#' @param field_definition_number integer of length 1. This is the field 
#' number we're checking the existence of in the FIT specification.
#' @param global_message_number integer of length 1. Specifies the global 
#' message number for the message type we're looking at.
#' 
#' @keywords internal
.isKnownField <- function(field_definition_number, global_message_number) {
  
  global_message_name <- .translateGlobalMessageNumber(global_message_number)
  field_definition_number %in% fit_message_types[[ global_message_name ]]$key

}

.field_list_to_tibble <- function(message, names) {
  attributes(message@fields) <- list(names = names)
  return(message@fields)
}

#' @importFrom dplyr bind_rows select mutate across everything cur_column
.processFieldsList <- function(x, global_message_number) {

    ## Message number 78 is 'hrv' which is special.  It contains only a single
    ## array of integers, but the length of the array is variable.
    ## We want to concatenate these, rather than grouping by message signature
    unlist <- global_message_number == 78
  
    names <- fieldDefinition(x[[1]])$field_def_num

    message_table <- lapply(x, FUN = .field_list_to_tibble, names = names)
    
    if(unlist) {
      message_table <- tibble(unlist( message_table) )
      colnames(message_table) <- names
    } else {
        message_table <- dplyr::bind_rows( message_table )
      # message_table <- tryCatch({
      #   dplyr::bind_rows( message_table )},
      #   error = function(e) {
      #     
      #     types <- lapply(message_table, function(x) sapply(x, class))
      #     types <- dplyr::bind_rows(types)
      #     types <- sapply(names(types), function(x) {
      #       x <- types[[x]]
      #       x <- x[!is.na(x)]
      #       if(any(x == "POSIXct")) return("POSIXct")
      #       if(any(x == "character")) return("character")
      #       if(any(x == "integer")) return("integer")
      #       "numeric"
      #     })
      #     
      #     message_table <- lapply(message_table, function(x) {
      #       for(y in names(x)) {
      #         x[[y]] <- methods::as(x[[y]], types[names(types) == y])
      #       }
      #       x
      #     })
      #     
      #     dplyr::bind_rows(message_table)
      #     
      #   })
    }
  
  ## some columns are not defined in the FIT profile.  We remove them here
  keep_idx <- vapply(as.integer(names(message_table)), 
                     FUN = .isKnownField, 
                     FUN.VALUE = logical(1), 
                     global_message_number = global_message_number)
  
  if(all(keep_idx == FALSE)) {
    stop("We have created an empty data.frame.  This should not happen!")
  }
  
  message_table <- message_table %>%
    dplyr::select(which(keep_idx)) %>%
    mutate(across(everything(), 
                  ~ .applyScaleAndOffset(input = ., 
                                         as.integer(cur_column()), 
                                         global_message_number) 
    )) %>%
    mutate(across(everything(), 
                  ~ .applyFormatConversions(input = ., 
                                         as.integer(cur_column()), 
                                         global_message_number) 
    )) %>%
    as_tibble()
  
  names(message_table) <- vapply( as.integer(names(message_table)),
                                  FUN = .translateField2, 
                                  FUN.VALUE = character(1),
                                  global_message_number )
  
  message_table <- .fixGarminProducts(message_table)
  
  if(hasDeveloperData(x[[1]])) {
    developer_message_table <- .processDevFieldsList(x)
    message_table <- bind_cols(message_table, developer_message_table)
  }
  
  return(message_table)
}


.processDevFieldsList <- function(x) {
    
  dev_msg_defs <- x[[1]]@dev_field_details
    
  field_names <- vapply(dev_msg_defs, .getValueForFieldNum, 3L, FUN.VALUE = character(1))
  units <- vapply(dev_msg_defs, .getValueForFieldNum, 8L, FUN.VALUE = character(1))
    
  message_table <- bind_rows(
    lapply(x, 
           FUN = function(y) {
             tmp <- y@dev_fields
             names(tmp) <- field_names
             return(tmp)
           } 
    ) 
  )

  for(i in ncol(message_table)) {
    attributes(message_table[[i]]) <- list(units = units[i])
  }
  
  return(message_table)
}
