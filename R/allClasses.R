
setClass("FitMessage",
         representation(
             global_message_number = "integer",
             field_definition = "data.frame",
             messages = "data.frame"
         ))

setClass(Class = "FitMessageHeader",
         representation(
             is_definition = "logical",
             is_developer_data = "logical",
             local_message_number = "integer",
             time_offset = "numeric"
         ))

setClass("FitDefinitionMessage",
         representation(
             header = "FitMessageHeader",
             is_little_endian = "logical",
             global_message_number = "integer",
             field_defs = "list",
             dev_field_defs = "list"
         ))

setClass("FitDataMessage",
         representation(
             header = "FitMessageHeader",
             definition = "FitDefinitionMessage",
             fields = "list"
         ))


#' @exportClass FitFile 
setClass("FitFile", 
         representation(
           header = "list", 
           file_id = "FitMessage",
           events = "FitMessage",
           records = "FitMessage",
           laps = "FitMessage"
         )
)

setMethod("show", signature = "FitFile", function(object) {
  
  cat("Fit File Data\n")
  cat("Device: ", object@file_id@messages$manufacturer, " ", object@file_id@messages$product, "\n")
  cat("Date: ", as.character(object@file_id@messages$time_created), "\n")

})


