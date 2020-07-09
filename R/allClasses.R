
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
           laps = "FitMessage"
         )
)

setMethod("show", signature = "FitFile", function(object) {
  
  cat("Fit File Data\n")
  cat("Device: ", object@file_id@messages$manufacturer, " ", object@file_id@messages$product, "\n")
  cat("Date: ", as.character(object@file_id@messages$time_created), "\n")

})


