########################
## Local Message Number
########################

setMethod("localMessageNumber", 
          signature = "FitMessageHeader",
          function(object) {
              if(object@is_compressed)
                  .binaryToInt(object@raw_rep[6:7])
              else
                  .binaryToInt(object@raw_rep[1:4])
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