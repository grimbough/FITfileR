.processFieldDefs <- function(fields) {
    
    fields <- as.integer(fields)
    fields <- split(fields, rep(1:3, by = length(fields)/3))
    #fields[[3]] <- format(as.hexmode(fields[[3]]), width = 2)
    fields[[3]] <- unlist(
        lapply(fields[[3]], function(x) { 
            FITfileR:::.binaryToInt(intToBits(x)[1:4])  
        })
    )
    names(fields) = c('field_def_num', 'size', 'base_type')
    return(fields)
}

.processDevFieldDefs <- function(fields) {
    
    fields <- as.integer(fields)
    fields <- split(fields, rep(1:3, by = length(fields)/3))
    names(fields) = c('field_num', 'size', 'developer_idx')
    return(fields)
}

.readMessage_definition <- function(con, message_header) {
    
    reserved <- readBin(con = con, what = "raw", n = 1, size = 1)
    architecture <- ifelse(readBin(con = con, what = "int", n = 1, size = 1),
                           "big", "little")
    global_message_num <- readBin(con = con, what = "int", n = 1, size = 2,
                                  endian = architecture, signed = FALSE)
    n_fields <- readBin(con = con, what = "int", n = 1, size = 1, signed = FALSE)
    field_definition <- .processFieldDefs(
        readBin(con = con, what = "raw", n = 3 * n_fields, size = 1, signed = FALSE)
    )
    if(hasDeveloperData(message_header)){
        ## do something with the developer fields
        n_dev_fields <- readBin(con = con, what = "int", n = 1, size = 1, signed = FALSE)
        dev_fields_raw <- readBin(con = con, what = "raw", n = 3 * n_dev_fields, size = 1, signed = FALSE)
        dev_field_definition <- .processDevFieldDefs(
            dev_fields_raw
        )
    } else {
        dev_field_definition = NULL
    }
    
    message <- new("FitDefinitionMessage",
                   header = message_header,
                   is_little_endian = (architecture == "little"),
                   global_message_number = global_message_num,
                   field_defs = field_definition,
                   dev_field_defs = dev_field_definition
    )
    
    return(message)
}

.readMessage_devdata <- function(con, header, definition, developer_msgs) {
    
    fieldDefs <- fieldDefinition(definition)
    ## we add 1 here because R indexing starts at 1 not 0
    fieldTypes <- fieldDefs$base_type + 1
    sizes <- fieldDefs$size
    devFieldDefs <- devFieldDefinition(definition)
    
    if(definition@is_little_endian) { endian <- "little" } else { endian <- "big" }
    
    message <- mapply(FUN = .readFields, fieldTypes, sizes, 
                      MoreArgs = list(definition = definition, con = con), 
                      SIMPLIFY = FALSE,
                      USE.NAMES = FALSE)
    
    dev_data_mesg_defs <- vector(mode = "list", length = length(devFieldDefs$field_num))
    baseTypes <- sizes <- integer(length = length(devFieldDefs$field_num))
    ## loop over the developer fields
    for(i in seq_along(devFieldDefs$field_num)) {
        
        ## index within the set of developer messages
        idx <- devFieldDefs$developer_idx[i] + 1
        field_num <- devFieldDefs$field_num[i] + 1
        
        developer_msg <- developer_msgs[[idx]]$messages[[field_num]]
        dev_data_mesg_defs[[i]] <- developer_msg
        dm_fieldDefs <- fieldDefinition(developer_msg)
        
        sizes[i] <- devFieldDefs$size[i]
        encodedType <- developer_msg@fields[[which(dm_fieldDefs$field_def_num == 2)]] %>% 
            as.integer() 
        baseTypes[i] <- .binaryToInt(intToBits(encodedType)[1:4])
    }
    
    ## we add 1 here because R indexing starts at 1 not 0
    baseTypes <- baseTypes + 1
        
    dev_data <- mapply(FUN = .readFields, baseTypes, sizes, 
                          MoreArgs = list(definition = definition, con = con), 
                          SIMPLIFY = FALSE,
                          USE.NAMES = FALSE)
    
    res <- new("FitDataMessageWithDevData",
               header = header,
               definition = definition,
               fields = message,
               dev_fields = dev_data,
               dev_field_details = dev_data_mesg_defs)
}

.readMessage_data <- function(con, header, definition) {
    
    fieldDefs <- fieldDefinition(definition)
    ## we add 1 here because R indexing starts at 1 not 0
    fieldTypes <- fieldDefs$base_type + 1
    sizes <- fieldDefs$size
    
    if(any(fieldTypes > length(data_type_lookup))) {
        stop("Unable to read data message.\n",
             "Unknown field types detected.")
    }
    
    message <- mapply(FUN = .readFields, fieldTypes, sizes, 
                      MoreArgs = list(definition = definition, con = con), 
                      SIMPLIFY = FALSE,
                      USE.NAMES = FALSE)
    
    res <- new("FitDataMessage",
               header = header,
               definition = definition,
               fields = message
    )
    
    return(res)
}

.readFields <- function(fieldType, sizes, definition, con) {
    
    if(definition@is_little_endian) { endian <- "little" } else { endian <- "big" }
    readInfo <- data_type_lookup[[ fieldType ]]
    
    ## a single field can have an array of values 
    single_size <- prod(readInfo$size, readInfo$n)
    n_values <- sizes %/% single_size
    
    if(fieldType == 8L) {
        suppressWarnings(
            res <- readChar(con = con, nchars = n_values, useBytes = TRUE)
        )
    } else {
        for(j in seq_len( n_values ) ) {
            
            dat <- readBin(con, what = readInfo$what, signed = readInfo$signed,
                           size = readInfo$size, n = readInfo$n, 
                           endian = endian)
            
            ## if we have unsigned ints, turn the bits into a numeric
            if(fieldType == 7L || fieldType == 13L) {
                if(definition@is_little_endian) {
                    bits <- as.logical(rawToBits(dat[1:4]))
                } else {
                    bits <- as.logical(rawToBits(dat[4:1]))
                }
                dat <- sum(2^(.subset(0:31, bits)))
            } else if (fieldType == 14L) { ## maybe this conversion should be done when reading?
                dat <- as.integer(dat)
            } else if (fieldType %in% c(15L, 16L)) {
                dat <- .rawToInt64(raw = dat)
            }
            
            if(n_values == 1) {
                res <- dat
            } else { ## put multiple values in a list, otherwise the tibble has columns with different lengths.
                if(j == 1) {
                    res <- list(dat)
                } else {
                    res[[1]] <- c(res[[1]], dat)
                }
            }
        }
    }
    return(res)
}

.readMessage_dev_data_id <- function(con, header, definition, devMessages) {
    tmp <- .readMessage_data(con = con, header = header, definition = definition)
    dev_data_idx_idx <- which(tmp@definition@field_defs$field_def_num == 3)
    manufacturer_id_idx <- which(tmp@definition@field_defs$field_def_num == 2)
    
    ## add 1 becuase FIT file indices are 0-based
    idx <- tmp@fields[[ dev_data_idx_idx ]]+1
    
    devMessages[[idx]] <- list()
    devMessages[[idx]][["messages"]] <- list()
    
    return(devMessages)
}

.readMessage_dev_data_field_definition <- function(con, header, definition, devMessages) {
    msg <- .readMessage_data(con = con, header = header, definition = definition)
    developer_idx <- which(msg@definition@field_defs$field_def_num == 0)
    field_idx <- which(msg@definition@field_defs$field_def_num == 1)
    
    ## add 1 becuase FIT file indices are 0-based
    dev_data_idx <- as.integer(msg@fields[[ developer_idx ]]) + 1
    field_number <- as.integer(msg@fields[[ field_idx ]]) + 1
    
    devMessages[[ dev_data_idx ]][[ "messages" ]][[ field_number ]] <- msg
    return(devMessages)
}
