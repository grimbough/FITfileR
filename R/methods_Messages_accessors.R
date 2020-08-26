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