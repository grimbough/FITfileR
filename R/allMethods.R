
setGeneric("hasDeveloperData", function(object) {
    standardGeneric("hasDeveloperData")
})

setMethod("hasDeveloperData", 
          signature = c("FitDataMessage"),
          function(object) {
              return(object@definition@header@has_developer_data)
          }
)

## Show
setMethod("show", 
          signature = "FitFile", 
          function(object) {
              cat("Fit File\n")
              if("file_id" %in% getMessageTypes(object)) {
                  file_id <- getMessagesByType(object, "file_id") 
                  cat("  File created: ", as.character(file_id$time_created), "\n", sep = "")
                  cat("  Device: ", file_id$manufacturer[1], " ", file_id$product[1], "\n", sep = "")
              }
              cat("  Number of data messages: ", length(object), sep = "")
          }
)

setMethod("show", signature = "FitDataMessage", function(object) {
    
    cat("Local message type: ", localMessageNumber(object),  " ", sep = "")
    cat("(message name: ", .translateGlobalMessageNumber( globalMessageNumber(object) ), ", ",  sep = "")
    cat("message type: ", object@definition@global_message_number, ", ", sep = "")
    cat("fields: ", nrow(object@definition@field_defs), ", bytes: ", sum(object@definition@field_defs$size), ")", sep = "")
    
    for(field in object@definition@field_defs$field_def_num) {
        translated <- .translateField(field, object@definition@global_message_number)
        cat("\n ", translated$value, " (", translated$key, ", ", translated$type, "):", sep = "")
        
        original <- object@fields[[ paste(field) ]] %>% unlist()
        adjusted <- .applyScaleAndOffset( original, field, object@definition@global_message_number ) 
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
    cat("message type: ", object@definition@global_message_number, ", ", sep = "")
    cat("fields: ", nrow(object@definition@field_defs), ", bytes: ", sum(object@definition@field_defs$size), ")", sep = "")
    
    for(field in object@definition@field_defs$field_def_num) {
        translated <- .translateField(field, object@definition@global_message_number)
        cat("\n ", translated$value, " (", translated$key, ", ", translated$type, "):", sep = "")
        
        original <- object@fields[[ paste(field) ]] %>% unlist()
        adjusted <- .applyScaleAndOffset( original, field, object@definition@global_message_number ) 
        units <- ifelse(is.na(translated$units), "", paste0(" ", translated$units))
        
        if(length(original) > 1) { cat(" {") }
        for(i in seq_along(original)) {
            cat(" ", adjusted[i], units, " (", original[i], ")", sep = "")
        }
        if(length(original) > 1) { cat(" }") }
        
    }
    
    for(field in object@definition@dev_field_defs$field_def_num) {
        translated <- filter(object@dev_field_details, field_def_num == field)
        cat("\n ", translated$field_name, ": ", sep = "")
        original <- object@dev_fields[[ paste(field) ]] %>% unlist()
        cat(original, translated$units)
    }
    
    
})

## Dump

setGeneric("dump", function(object) {
    standardGeneric("dump")
})

setMethod("dump", signature = "FitFile",
          function(object) {
              object@messages
          }
)


