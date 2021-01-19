
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
    cat("fields: ", nrow(field_defs), ", bytes: ", sum(field_defs$size), ")", sep = "")
    
    for(field in field_defs$field_def_num) {
        translated <- .translateField(field, globalMessageNumber( object ))
        cat("\n ", translated$value, " (", translated$key, ", ", translated$type, "):", sep = "")
        
        original <- object@fields[[ paste(field) ]] %>% unlist()
        adjusted <- .applyScaleAndOffset( original, field, globalMessageNumber( object ) ) 
        units <- ifelse(is.na(translated$units), "", paste0(" ", translated$units))
        
        if(length(original) > 1) { cat(" {") }
        for(i in seq_along(original)) {
            cat(" ", adjusted[i], units, " (", original[i], ")", sep = "")
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
    
    for(field in field_defs$field_def_num) {
        translated <- .translateField(field, globalMessageNumber( object ))
        cat("\n ", translated$value, " (", translated$key, ", ", translated$type, "):", sep = "")
        
        original <- object@fields[[ paste(field) ]] %>% unlist()
        adjusted <- .applyScaleAndOffset( original, field, globalMessageNumber( object ) ) 
        units <- ifelse(is.na(translated$units), "", paste0(" ", translated$units))
        
        if(length(original) > 1) { cat(" {") }
        for(i in seq_along(original)) {
            cat(" ", adjusted[i], units, " (", original[i], ")", sep = "")
        }
        if(length(original) > 1) { cat(" }") }
        
    }
    
    for(field in object@definition@dev_field_defs$field_def_num) {
        idx <- which(object@dev_field_details$field_def_num == field)
        cat("\n ", object@dev_field_details$field_name[idx], ": ", sep = "")
        original <- object@dev_fields[[ paste(field) ]] %>% unlist()
        cat(original, object@dev_field_details$units)
    }
    
    
})





