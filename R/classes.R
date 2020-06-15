
setClass("FitMessage",
         representation(
             global_message_number = "integer",
             field_definition = "data.frame",
             messages = "data.frame"
         ))


#' @exportClass FitFile 
setClass("FitFile", 
         representation(
           header = "list", 
           file_id = "FitMessage",
           events = "FitMessage",
           records = "FitMessage",
           lap = "FitMessage"
         )
)

setMethod("show", signature = "FitFile", function(object) {
  
  cat("Fit File Data\n")
  cat("Device: ", object@file_id$manufacturer, " ", object@file_id$product, "\n")
  cat("Date: ", object@file_id$time_created, "\n")

})


