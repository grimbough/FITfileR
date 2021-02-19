## fit time stamps are from 31st December 1989
## this function transforms them into data/times
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
      message_table$product[garmin_idx] <- .getFromDataTypeFactor(values = message_table$product[garmin_idx], 
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
      input[[1]] <- input[[1]] / as.numeric(details$scale[1])
    } else {
      input <- input / as.numeric(details$scale[1])
    }
  }
  
  ## subtract offset if it exists
  if(!is.na(details$offset[1])) {
      input <- input - details$offset[1]
  }
  
  ## add units as attribute
  if(!is.na(details$units[1])) {
      attributes(input) <- list(units = details$units)
  }
  
  return( input )
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
    input <- input * (180 / 2^31)
    attributes(input) <- list(units = "degrees")
  } else if(type %in% names(fit_data_types)) {
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

#' @importFrom dplyr bind_rows select mutate across everything cur_column
.processFieldsList <- function(x, global_message_number) {

    ## Message number 78 is 'hrv' which is special.  It contains only a single
    ## array of integers, but the length of the array is varable.
    ## We want to concatenate these, rather than grouping by message signature
    unlist <- global_message_number == 78
  
    names <- fieldDefinition(x[[1]])$field_def_num
    message_table <- lapply(x, 
                            FUN = function(y, names, unlist) {
                              attributes(y@fields) <- list(names = names) 
                              if(unlist)
                                return(y@fields)
                              else
                                return(unlist(y@fields))
                            }, names = names, unlist = unlist 
    ) 
    
    if(unlist) {
      message_table <- tibble(unlist( message_table) )
      colnames(message_table) <- names
    } else {
      message_table <- dplyr::bind_rows( message_table )
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
    message_table <- bind_cols(message_table, .processDevFieldsList(x))
  }
  
  return(message_table)
}


.processDevFieldsList <- function(x) {
  message_table <- lapply(x, 
                          FUN = function(y) {
                            y@dev_fields
                          } 
  ) %>% 
    dplyr::bind_rows( ) 
  
  names(message_table) <- x[[1]]@dev_field_details$field_name
  
  for(i in ncol(message_table)) {
    attributes(message_table[[i]]) <- list(units = x[[1]]@dev_field_details$units)
  }
  
  return(message_table)
}
