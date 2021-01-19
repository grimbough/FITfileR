#' S4 classes representing various aspects of FIT messages.
#' 
#' @slot global_message_number A integer of length 1.
#' @slot field_definition A data.frame.
#' @slot messages A data.frame.
#' @name FitMessages
NULL 

#' The \code{FitMessageHeader} class represents the single-byte header that
#' precedes all FIT messages.  It forms a part of all 
#' \code{FitDefinitionMessage} and \code{FitDataMessage} instances.
#' 
#' @slot is_defintion Logical indicating if this is a definition message.
#' @slot has_developer_data Logical defining whether the message contains 
#' developer data.
#' @slot local_message_number The 'local' message number for this message
#' type.
#' @slot time_offset Numeric (length 1) giving the time offset for messages
#' with compressed time stamps.
#' @slot raw_rep Raw (length 1) matching the single byte that defined this
#' message header.  Used internally as a quick comparison to see if this is the
#' same header as the previous message to speed up file reading.
setClass(Class = "FitMessageHeader",
         slots = c(
             raw_rep = "raw",
             is_compressed = "logical"
         ))

#' The \code{FitDefinitionMessage} class represents definition messages.
#' 
#' @rdname FitMessages 
setClass("FitDefinitionMessage",
         slots = c(
             header = "FitMessageHeader",
             is_little_endian = "logical",
             global_message_number = "integer",
             field_defs = "data.frame",
             dev_field_defs = "ANY",
             .signature = "character"
         ))

#' The \code{FitDataMessage} class holds data messages.
#' 
#' @slot header An object of class \code{FitMessageHeader}.
#' @slot definition An object of class \code{FitDefinitionMessage}.
#' @slot fields A list containing the data encoded in this message.
#' 
#' @rdname FitMessages 
setClass("FitDataMessage",
         slots = c(
             header = "FitMessageHeader",
             definition = "FitDefinitionMessage",
             fields = "list"
         ))

#' The \code{FitDataMessageWithDevData} class extends \code{FitDataMessage} 
#' with additional slots to store the definitions of developer data fields that
#' are not defined in the standard FIT file specification.
#' 
#' @rdname FitMessages
#' @include class_validity.R
setClass("FitDataMessageWithDevData",
         slots = c(
             dev_fields = "list",
             dev_field_details = "list"
         ),
         contains = "FitDataMessage", 
)
setValidity("FitDataMessageWithDevData", validDevData)

#' An S4 class representing a FIT file
#' 
#' @slot header A list containing details of the file header
#' @slot messages A list of [FitDataMessage-class]s
#' 
#' @exportClass FitFile 
setClass("FitFile", 
         slots = c(
           header = "list", 
           messages = "list"
         )
)




