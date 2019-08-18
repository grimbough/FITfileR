.processFieldDefs <- function(fields) {
    
    fields <- as.integer(fields)
    fields <- split(fields, rep(1:3, by = length(fields)/3))
    names(fields) = c('field_def_num', 'size', 'base_type')
    fields <- as_tibble(fields) %>%
        mutate(base_type = format(as.hexmode(base_type), width = 2))
    return(fields)
}

.readMessage.definition <- function(con, devFields = FALSE) {
    
    message <- list()
    message$reserved <- readBin(con = con, what = "raw", n = 1, size = 1)
    message$architecture <- ifelse(readBin(con = con, what = "int", n = 1, size = 1),
                                   "big", "little")
    message$global_message_num <- readBin(con = con, what = "int", n = 1, size = 2,
                                          endian = message$architecture)
    message$n_fields <- readBin(con = con, what = "int", n = 1, size = 1)
    #message( message$n_fields )
    message$field_definition <- .processFieldDefs(
        readBin(con = con, what = "raw", n = 3 * message$n_fields, size = 1)
    )
    if(devFields){
        ## do something with the developer fields
        ## currently not supported
        messages$n_dev_fields <- readBin(con = con, what = "int", n = 1, size = 1)
        message$dev_field_definition <- .processFieldDefs(
            readBin(con = con, what = "raw", n = 3 * message$n_dev_fields, size = 1)
        )
    }
    return(list(message = message))
}


.readMessage.data <- function(con, definition, compressed_timestamp = FALSE) {
    
    fieldTypes <- definition$field_definition$base_type
    sizes <- definition$field_definition$size
    
    message <- list()
    for(i in seq_along(fieldTypes)) {
        
        if( fieldTypes[i] %in% names(data_type_lookup) ) {
        
        readInfo <- data_type_lookup[[ fieldTypes[i] ]]
        
        ## a single field can have an array of values 
        single_size <- prod(as.integer(readInfo[3:4]))
        for(j in seq_len( sizes[i] %/% single_size ) ) {
            message[[i]] <- readBin(con, what = readInfo[[1]], signed = readInfo[[2]],
                                    size = readInfo[[3]], n = readInfo[[4]], 
                                    endian = definition$architecture)
            
            ## if we have unsigned ints, turn the bits into a numeric
            if(fieldTypes[i] %in% c('86', '8c')) {
                if(definition$architecture == "little") {
                    bits <- as.logical(rawToBits(message[[i]][1:4]))
                } else {
                    bits <- as.logical(rawToBits(message[[i]][4:1]))
                }
                message[[i]] <- sum(2^(.subset(0:31, bits)))
            }
        }
        } else {
            ## we end up here if the datatype is not defined in the FIT spec
            #message("unknown data type")
            readBin(con, what = "integer", size = 1, n = sizes[i])
            message[[i]] <- 0
        }
    }
    
    #message <- as.data.frame(message)
    message <- structure(message, row.names = c(NA, -1), class = "data.frame")
    colnames(message) <- definition$field_definition$field_def_num
    return(list(message = message))
}

