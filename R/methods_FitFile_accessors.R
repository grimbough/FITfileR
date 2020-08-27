#' Extracting messages from Fit Files
#' 
#' Methods for retrieving messages from \code{\link{FitFile-class}} objects.
#' 
#' The FIT file specification allows for a large number of message types.  
#' FitFileR provides accessor methods for some of the most common. These 
#' include `records()` and `laps()`.
#'
#' @param fitFile A \code{\link{FitFile-class}} object.
#' @param global_message_number Integer specifying the message number of 
#' the message type wanted.
#' @param message_type Either an integer or character of length 1, specifying 
#' either a global message number or message type respectively.
#' @name fitfile_accessors
#' @include allClasses.R
NULL

########################################
## List Message types found in a file ##
########################################

#' List the names of the messages types found in a FitFile object
#' @rdname fitfile_accessors
#' @export
setGeneric("getMessageTypes", function(fitFile, global_message_number) {
    standardGeneric("getMessageTypes")
})

#' @rdname fitfile_accessors
#' @importFrom dplyr filter
#' @importFrom magrittr %>% extract2
setMethod("getMessageTypes", 
          signature = c("FitFile"),
          function(fitFile) {
              all_gmn <- vapply(fitFile@messages, FUN = globalMessageNumber, 
                                FUN.VALUE = integer(1))
              filter(fit_data_types$mesg_num, key %in% unique(all_gmn)) %>% 
                  magrittr::extract2('value')
          })

############################################################
## Extract messages based on global message number / name ##
############################################################

#' @rdname fitfile_accessors
#' @export
setGeneric("getMessagesByType", function(fitFile, message_type) {
    standardGeneric("getMessagesByType")
})

setMethod("getMessagesByType", 
          signature = c("FitFile", "integer"),
          function(fitFile, message_type) {
              
              idx <- vapply(fitFile@messages, FUN = globalMessageNumber, 
                            FUN.VALUE = integer(1)) == message_type
              
              if(length(idx)) {
                  messages <- fitFile@messages[ idx ]
                  
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

setMethod("getMessagesByType", 
          signature = c("FitFile", "character"),
          function(fitFile, message_type) {
              types_in_file <- getMessageTypes(fitFile)
              if(!message_type %in% types_in_file) {
                  stop("Message type ", message_type, " not found in file")
              }
              global_message_number <- .translateGlobalMessageName(message_type)
              
              getMessagesByType(fitFile, global_message_number)
              
          }
)


#########################################
## Accessors for common messages types ##
#########################################

#' @rdname fitfile_accessors
#' @export
setGeneric("records", function(fitFile) {
    standardGeneric("records")
})

setMethod("records", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 20L)
          }
)

#' @rdname fitfile_accessors
#' @export
setGeneric("laps", function(fitFile) {
    standardGeneric("laps")
})

setMethod("laps", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 19L)
          }
)
