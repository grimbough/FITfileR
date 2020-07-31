#' @export
setGeneric("getMessagesByType", function(object, global_message_number) {
    standardGeneric("getMessagesByType")
})


#' @import dplyr
#' @importFrom magrittr %>%
setMethod("getMessagesByType", 
          signature = c("RawFitFile", "integer"),
          function(object, global_message_number) {
              
              idx <- vapply(object@messages, FUN = globalMessageNumber, 
                            FUN.VALUE = integer(1)) == global_message_number
              
              if(length(idx)) {
                  messages <- object@messages[ idx ]
                  
                  signatures <- vapply(messages, 
                                       function(x) { x@definition@.signature }, 
                                       FUN.VALUE = character(1))
                  
                  messages2 <- split(messages, signatures)
                  
                  messages3 <- lapply(messages2, FUN = .processFieldsList, global_message_number)
                  
                  if(length(messages3) == 1) {
                      messages3 <- messages3[[1]]
                  } else {
                      gm_name <- fitFileR:::.translateGlobalMessageNumber( global_message_number )
                      names(messages3) <- paste(gm_name, seq_along(messages3), sep = "_")
                  }
                  return(messages3)
              } else {
                  return(NULL)
              }
              
          }
)


#' @export
setGeneric("localMessageNumber", function(object) {
    standardGeneric("localMessageNumber")
})

setMethod("localMessageNumber", 
          signature = "FitMessageHeader",
          function(object) {
              object@local_message_number
          }
)

setMethod("localMessageNumber", 
          signature = "FitDefinitionMessage",
          function(object) {
              localMessageNumber(object@header)
          }
)

setMethod("localMessageNumber", 
          signature = "FitDataMessage",
          function(object) {
              localMessageNumber(object@definition)
          }
)

########################
## Global Message Number
########################

#' @export
setGeneric("globalMessageNumber", function(object) {
    standardGeneric("globalMessageNumber")
})

setMethod("globalMessageNumber", 
          signature = "FitDefinitionMessage",
          function(object) {
              object@global_message_number
          }
)

setMethod("globalMessageNumber", 
          signature = "FitDataMessage",
          function(object) {
              globalMessageNumber(object@definition)
          }
)

## Show
setMethod("show", 
          signature = "RawFitFile", 
          function(object) {
              cat("Raw Fit File\n")
              cat("Number of messages: ", length(object@messages), sep = "")
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

## Dump

setGeneric("dump", function(object) {
    standardGeneric("dump")
})

setMethod("dump", signature = "RawFitFile",
          function(object) {
              object@messages
          }
)



