.processFieldDefs <- function(fields) {
    
    fields <- as.integer(fields)
    fields <- split(fields, rep(1:3, by = length(fields)/3))
    names(fields) = c('field_def_num', 'size', 'base_type')
    fields <- as_tibble(fields) %>%
        mutate(base_type = format(as.hexmode(base_type), width = 2))
    return(fields)
}

.readMessage_definition <- function(con, message_header) {
    
    reserved <- readBin(con = con, what = "raw", n = 1, size = 1)
    architecture <- ifelse(readBin(con = con, what = "int", n = 1, size = 1),
                           "big", "little")
    global_message_num <- readBin(con = con, what = "int", n = 1, size = 2,
                                  endian = architecture)
    n_fields <- readBin(con = con, what = "int", n = 1, size = 1)
    field_definition <- .processFieldDefs(
        readBin(con = con, what = "raw", n = 3 * n_fields, size = 1, signed = FALSE)
    )
    if(hasDeveloperData(message_header)){
        ## do something with the developer fields
        n_dev_fields <- readBin(con = con, what = "int", n = 1, size = 1, signed = FALSE)
        dev_field_definition <- .processFieldDefs(
            readBin(con = con, what = "raw", n = 3 * n_dev_fields, size = 1)
        )
    } else {
        dev_field_definition = NULL
    }
    
    message <- new("FitDefinitionMessage",
                   header = message_header,
                   is_little_endian = (architecture == "little"),
                   global_message_number = global_message_num,
                   field_defs = field_definition,
                   dev_field_defs = dev_field_definition,
                   .signature = .definitionSignature(field_definition)
    )
    
    return(message)
}

## should be refactored!!!
## currently just copy/paste from .readMessage_data()
.readMessage_devdata <- function(con, field_defs, is_little_endian) {
    
    fieldTypes <- field_defs$base_type
    sizes <- field_defs$size
    
    message <- vector(mode = "list", length = length(fieldTypes))
    for(i in seq_along(fieldTypes)) {
        
        if( fieldTypes[i] %in% names(data_type_lookup) ) {
            
            readInfo <- data_type_lookup[[ fieldTypes[i] ]]
            
            ## a single field can have an array of values 
            single_size <- prod(as.integer(readInfo[3:4]))
            
            n_values <- sizes[i] %/% single_size
            if(fieldTypes[i] == "07") {
                suppressWarnings(
                    message[[i]] <- readChar(con = con, nchars = n_values, useBytes = TRUE)
                )
            } else {
                for(j in seq_len( n_values ) ) {
                    
                    dat <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                                   size = readInfo[[3]], n = readInfo[[4]], 
                                   endian = ifelse(is_little_endian, "little", "big"))
                    
                    ## if we have unsigned ints, turn the bits into a numeric
                    if(fieldTypes[i] %in% c('86', '8c')) {
                        if(is_little_endian) {
                            bits <- as.logical(rawToBits(dat[1:4]))
                        } else {
                            bits <- as.logical(rawToBits(dat[4:1]))
                        }
                        dat <- sum(2^(.subset(0:31, bits)))
                    } else if (fieldTypes[i] == '0d') { ## maybe this conversion should be done when reading?
                        dat <- as.integer(dat)
                    }
                    
                    if(n_values == 1) {
                        message[[i]] <- c(message[[i]], dat)
                    } else { ## put multiple values in a list, otherwise the tibble has columns with different lengths.
                        if(j == 1) {
                            message[[i]] <- list(dat)
                        } else {
                            message[[i]][[1]] <- c(message[[i]][[1]], dat)
                        }
                    }
                }
            }
        } else {
            ## we end up here if the datatype is not defined in the FIT spec
            #message("unknown data type")
            readBin(con, what = "integer", size = 1, n = sizes[i])
            message[[i]] <- 0
        }
    }
    return(message)
}

.readMessage_data <- function(con, header, definition) {
    
    fieldDefs <- fieldDefinition(definition)
    fieldTypes <- fieldDefs$base_type
    sizes <- fieldDefs$size
    
    message <- vector(mode = "list", length = length(fieldTypes))
    for(i in seq_along(fieldTypes)) {
        
        if( fieldTypes[i] %in% names(data_type_lookup) ) {
            
            readInfo <- data_type_lookup[[ fieldTypes[i] ]]
            
            ## a single field can have an array of values 
            single_size <- prod(as.integer(readInfo[3:4]))
            
            n_values <- sizes[i] %/% single_size
            if(fieldTypes[i] == "07") {
                suppressWarnings(
                    message[[i]] <- readChar(con = con, nchars = n_values, useBytes = TRUE)
                )
            } else {
                for(j in seq_len( n_values ) ) {
                    
                    dat <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                                   size = readInfo[[3]], n = readInfo[[4]], 
                                   endian = ifelse(definition@is_little_endian, "little", "big"))
                    
                    ## if we have unsigned ints, turn the bits into a numeric
                    if(fieldTypes[i] == '86' || fieldTypes[i] == '8c') {
                        if(definition@is_little_endian) {
                            bits <- as.logical(rawToBits(dat[1:4]))
                        } else {
                            bits <- as.logical(rawToBits(dat[4:1]))
                        }
                        dat <- sum(2^(.subset(0:31, bits)))
                    } else if (fieldTypes[i] == '0d') { ## maybe this conversion should be done when reading?
                        dat <- as.integer(dat)
                    }
                    
                    if(n_values == 1) {
                        message[[i]] <- c(message[[i]], dat)
                    } else { ## put multiple values in a list, otherwise the tibble has columns with different lengths.
                        if(j == 1) {
                            message[[i]] <- list(dat)
                        } else {
                            message[[i]][[1]] <- c(message[[i]][[1]], dat)
                        }
                    }
                }
            }
        } else {
            ## we end up here if the datatype is not defined in the FIT spec
            readBin(con, what = "integer", size = 1, n = sizes[i])
            message[[i]] <- 0
        }
    }
    
    if(hasDeveloperData(definition)) {
        dev_data <- .readMessage_devdata(con, definition@dev_field_defs, definition@is_little_endian)
        names(dev_data) <- devFieldDefinition(definition)$field_def_num
        
        res <- new("FitDataMessageWithDevData",
                   header = header,
                   definition = definition,
                   fields = message,
                   dev_fields = dev_data
        )
    } else {
        res <- new("FitDataMessage",
                   header = header,
                   definition = definition,
                   fields = message
        )
    }
    
    return(res)
}

