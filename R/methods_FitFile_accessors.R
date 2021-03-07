#' Extracting messages from FIT Files
#' 
#' Methods for retrieving messages from \code{\link{FitFile-class}} objects.
#' 
#' The FIT file specification allows for a large number of message types.  
#' FITfileR provides accessor methods for some of the most common. These 
#' include `records()` and `laps()`.
#' 
#' If a predefined function doesn't exist for the message type you want to
#' extract, any message type can be retrieved with \code{getMessagesByType}.
#' The second argument can take either the global message number (as specified
#' in the FIT File definition) of message type you want, or the message name.
#' A list of names for the message types held in a \code{\link{FitFile-class}} 
#' object can be retrieved with \code{listMessageTypes}.
#' 
#' The return type is dependant upon whether the \code{\link{FitFile-class}} 
#' contains multiple message definitions for the same message type.  It is not 
#' uncommon for this to occur e.g. if a new sensor is added during an activity
#' the *records* field definition will change.  If there is a single definition
#' for the message type a `tibble` will be returned, otherwise a `list` of 
#' `tibble`s is returned.  The length of this `list` reflects the number of 
#' unique definitions for the message type within the file.  It may be 
#' straightforward to combine these `tibbles` e.g. via \code{\link{rbind}}, but
#' this is left to the user.
#' 
#' @return Either a `tibble` or a `list` of `tibble`. See `details` for more 
#' information.
#'
#' @param fitFile A \code{\link{FitFile-class}} object.
#' @param message_type Either an integer or character vector (length 1), 
#' specifying either a global message number or message type respectively.
#' @name FitFile-accessors
#' @include allClasses.R
NULL

########################################
## List Message types found in a file ##
########################################

#' List the names of the messages types found in a FitFile object
#' @rdname FitFile-accessors
#' @export
setGeneric("listMessageTypes", function(fitFile) {
    standardGeneric("listMessageTypes")
})

#' @rdname FitFile-accessors
#' @importFrom dplyr filter
#' @importFrom magrittr %>% extract2
setMethod("listMessageTypes", 
          signature = c("FitFile"),
          function(fitFile) {
              all_gmn <- vapply( messages(fitFile), FUN = globalMessageNumber, 
                                FUN.VALUE = integer(1))
              filter(fit_data_types$mesg_num, key %in% unique(all_gmn)) %>% 
                  magrittr::extract2('value')
          }
)

############################################################
## Extract messages based on global message number / name ##
############################################################

#' @rdname FitFile-accessors
#' @export
setGeneric("getMessagesByType", function(fitFile, message_type) {
    standardGeneric("getMessagesByType")
})

#' @rdname FitFile-accessors
setMethod("getMessagesByType", 
          signature = c("FitFile", "integer"),
          function(fitFile, message_type) {
              
              idx <- vapply(messages(fitFile), FUN = globalMessageNumber, 
                            FUN.VALUE = integer(1)) == message_type
              
              if(any(idx)) {
                  messages <- messages(fitFile)[ idx ]
                  
                  signatures <- vapply(messages, 
                                       function(x) { x@definition@.signature }, 
                                       FUN.VALUE = character(1))
                  
                  ## this 
                  if(message_type != 78) {
                      messages2 <- split(messages, signatures)
                  } else {
                      messages2 <- list(messages)
                  }
                  
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

#' @rdname FitFile-accessors
setMethod("getMessagesByType", 
          signature = c("FitFile", "character"),
          function(fitFile, message_type) {
              types_in_file <- listMessageTypes(fitFile)
              if(!message_type %in% types_in_file) {
                  stop("Message type ", message_type, " not found in file")
              }
              global_message_number <- .translateGlobalMessageName(message_type)
              
              getMessagesByType(fitFile, global_message_number)
              
          }
)

####################
## Slot accessors ##
####################


setGeneric("messages", function(fitFile) {
    standardGeneric("messages")
})

#' @rdname FitFile-accessors
setMethod("messages", signature = "FitFile",
          function(fitFile) {
              fitFile@messages
          }
)


#########################################
## Accessors for common messages types ##
#########################################

#' @rdname FitFile-accessors
#' @export
setGeneric("file_id", function(fitFile) {
    standardGeneric("file_id")
})

#' @rdname FitFile-accessors
setMethod("file_id", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 0L)
          }
)

#' @rdname FitFile-accessors
#' @export
setGeneric("records", function(fitFile) {
    standardGeneric("records")
})

#' @rdname FitFile-accessors
setMethod("records", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 20L)
          }
)

#' @rdname FitFile-accessors
#' @export
setGeneric("laps", function(fitFile) {
    standardGeneric("laps")
})

#' @rdname FitFile-accessors
setMethod("laps", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 19L)
          }
)

#' @rdname FitFile-accessors
#' @export
setGeneric("events", function(fitFile) {
    standardGeneric("events")
})

#' @rdname FitFile-accessors
setMethod("events", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 21L)
          }
)


#' @rdname FitFile-accessors
#' @export
setGeneric("hrv", function(fitFile) {
    standardGeneric("hrv")
})

#' @rdname FitFile-accessors
setMethod("hrv", signature = "FitFile",
          function(fitFile) {
              getMessagesByType(fitFile, message_type = 78L)
          }
)





