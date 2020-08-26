
setClass("FitMessage",
         representation(
             global_message_number = "integer",
             field_definition = "data.frame",
             messages = "data.frame"
         ))

setClass(Class = "FitMessageHeader",
         representation(
             is_definition = "logical",
             has_developer_data = "logical",
             local_message_number = "integer",
             time_offset = "numeric",
             raw_rep = "raw"
         ))

setClass("FitDefinitionMessage",
         representation(
             header = "FitMessageHeader",
             is_little_endian = "logical",
             global_message_number = "integer",
             field_defs = "data.frame",
             dev_field_defs = "ANY",
             .signature = "character"
         ))

setClass("FitDataMessage",
         representation(
             header = "FitMessageHeader",
             definition = "FitDefinitionMessage",
             fields = "list"
         ))

setClass("FitDataMessageWithDevData",
         representation(
             dev_fields = "list",
             dev_field_details = "data.frame"
         ),
         contains = "FitDataMessage"
)

#' An S4 class representing a FIT file
#' 
#' @slot header A list containing details of the file header
#' @slot messages A list of [FitDataMessage-class]s
#' 
#' @exportClass FitFile 
setClass("FitFile", 
         representation(
           header = "list", 
           messages = "list"
         )
)




