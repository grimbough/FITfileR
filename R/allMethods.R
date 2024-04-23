
## Show
setMethod("show", 
          signature = "FitFile", 
          function(object) {
              cat("Fit File\n")
              if("file_id" %in% listMessageTypes(object)) {
                  file_id <- getMessagesByType(object, "file_id") 
                  if("time_created" %in% names(file_id))
                    cat("\u251C\u2574File created: ", as.character(file_id$time_created), "\n", sep = "")
                  if(all(c("manufacturer", "product") %in% names(file_id)))
                    cat("\u251C\u2574Device: ", file_id$manufacturer[1], " ", file_id$product[1], "\n", sep = "")
              }
              cat("\u2514\u2574Number of data messages: ", length(object), sep = "")
          }
)

setMethod("show", signature = "FitMessageHeader", function(object) {
    cat("Definition: ", isDefinition(object))
} 
)

setMethod("show", signature = "FitDataMessage", function(object) {
    
    cat("Local message type: ", localMessageNumber(object),  " ", sep = "")
    cat("(message name: ", .translateGlobalMessageNumber( globalMessageNumber(object) ), ", ",  sep = "")
    cat("message type: ", globalMessageNumber( object ), ", ", sep = "")
    
    field_defs <- fieldDefinition(object)
    cat("fields: ", length(field_defs$field_def_num), ", bytes: ", sum(field_defs$size), ")", sep = "")
    
    for(i in seq_along(field_defs$field_def_num)) {
        field <- field_defs$field_def_num[i]
        translated <- .translateField(field, globalMessageNumber( object ))
        cat("\n ", translated$value, " (", translated$key, ", ", translated$type, "):", sep = "")
        
        original <- unlist( object@fields[[ i ]] )
        adjusted <- .applyScaleAndOffset( original, field, globalMessageNumber( object ) ) 
        units <- ifelse(is.na(translated$units), "", paste0(" ", translated$units))
        
        if(length(original) > 1) { cat(" {") }
        for(j in seq_along(original)) {
            cat(" ", adjusted[j], units, " (", original[j], ")", sep = "")
        }
        if(length(original) > 1) { cat(" }") }
        
    }
    
})

setMethod("show", signature = "FitDataMessageWithDevData", function(object) {
    
    cat("Local message type: ", localMessageNumber(object),  " ", sep = "")
    cat("(message name: ", .translateGlobalMessageNumber( globalMessageNumber(object) ), ", ",  sep = "")
    cat("message type: ", globalMessageNumber( object ), ", ", sep = "")
    
    field_defs <- fieldDefinition(object)
    cat("fields: ", nrow(field_defs), ", bytes: ", sum(field_defs$size), ")", sep = "")
    
    for(i in seq_along(field_defs$field_def_num)) {
        field <- field_defs$field_def_num[i]
        translated <- .translateField(field, globalMessageNumber( object ))
        cat("\n ", translated$value, " (", translated$key, ", ", translated$type, "):", sep = "")
        
        original <- unlist( object@fields[[ i ]] )
        adjusted <- .applyScaleAndOffset( original, field, globalMessageNumber( object ) ) 
        units <- ifelse(is.na(translated$units), "", paste0(" ", translated$units))
        
        if(length(original) > 1) { cat(" {") }
        for(j in seq_along(original)) {
            cat(" ", adjusted[j], units, " (", original[j], ")", sep = "")
        }
        if(length(original) > 1) { cat(" }") }
        
    }
    
    for(i in seq_along(object@definition@dev_field_defs$field_num)) {
        field <- object@definition@dev_field_defs$field_num[i]
        
        dev_idx <- object@definition@dev_field_defs$developer_idx[i]
        ## identify the correct developer message definition
        dev_msg_idxs <- vapply(object@dev_field_details, FUN = .getValueForFieldNum, 
               value = 0L, FUN.VALUE = integer(1))
        
        dev_msg_def <- object@dev_field_details[[ which(dev_msg_idxs == dev_idx) ]]
        
        name <- .getValueForFieldNum(dev_msg_def, value = 3L)
        units <- .getValueForFieldNum(dev_msg_def, value = 8L)
        field_def_num <- .getValueForFieldNum(dev_msg_def, value = 1L)
        
        cat(
        sprintf("\n %s (%i, %i): %s %s", 
                name, dev_idx, field_def_num, object@dev_fields[[i]], units)
        )
        
    }
    
})



