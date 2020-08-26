#########################
## Extracting messages ##
#########################

#' List the names of the messages types found in a FitFile object
#' @export
setGeneric("getMessageTypes", function(object, global_message_number) {
    standardGeneric("getMessageTypes")
})

#' @import dplyr
#' @importFrom magrittr %>%
setMethod("getMessageTypes", 
          signature = c("FitFile"),
          function(object) {
              all_gmn <- vapply(object@messages, FUN = globalMessageNumber, 
                                FUN.VALUE = integer(1))
              filter(fit_data_types$mesg_num, key %in% unique(all_gmn)) %>% 
                  magrittr::extract2('value')
          })

############################################################
## Extract messages based on global message number / name ##
############################################################

#' @include allClasses.R
#' @export
setGeneric("getMessagesByType", function(object, message_type) {
    standardGeneric("getMessagesByType")
})


#' @import dplyr
#' @importFrom magrittr %>%
setMethod("getMessagesByType", 
          signature = c("FitFile", "integer"),
          function(object, message_type) {
              
              idx <- vapply(object@messages, FUN = globalMessageNumber, 
                            FUN.VALUE = integer(1)) == message_type
              
              if(length(idx)) {
                  messages <- object@messages[ idx ]
                  
                  signatures <- vapply(messages, 
                                       function(x) { x@definition@.signature }, 
                                       FUN.VALUE = character(1))
                  
                  messages2 <- split(messages, signatures)
                  
                  messages3 <- lapply(messages2, FUN = .processFieldsList, message_type)
                  
                  if(length(messages3) == 1) {
                      messages3 <- messages3[[1]]
                  } else {
                      gm_name <- .translateGlobalMessageNumber( message_type )
                      names(messages3) <- paste(gm_name, seq_along(messages3), sep = "_")
                  }
                  return(messages3)
              } else {
                  return(NULL)
              }
              
          }
)

#' @import dplyr
#' @importFrom magrittr %>%
setMethod("getMessagesByType", 
          signature = c("FitFile", "character"),
          function(object, message_type) {
              types_in_file <- getMessageTypes(object)
              if(!message_type %in% types_in_file) {
                  stop("Message type ", message_type, " not found in file")
              }
              global_message_number <- .translateGlobalMessageName(message_type)
              
              getMessagesByType(object, global_message_number)
              
          }
)


#########################################
## Accessors for common messages types ##
#########################################

## records
#' @export
setGeneric("records", function(object) {
    standardGeneric("records")
})

setMethod("records", signature = "FitFile",
          function(object) {
              getMessagesByType(object, message_type = 20L)
          }
)

## laps
#' @export
setGeneric("laps", function(object) {
    standardGeneric("laps")
})

setMethod("laps", signature = "FitFile",
          function(object) {
              getMessagesByType(object, message_type = 19L)
          }
)
