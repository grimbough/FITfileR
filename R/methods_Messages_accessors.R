########################
## Local Message Number
########################

setMethod("localMessageNumber", 
          signature = "raw",
          function(object) {
              if(isCompressed(object)) 
                  .binaryToInt(rawToBits(object[6:7]))
              else
                  .binaryToInt(rawToBits(object)[1:4])
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

########################
## Definition
########################

setMethod("fieldDefinition", 
          signature = "FitDefinitionMessage",
          function(object) {
              object@field_defs
          }
)

setMethod("fieldDefinition", 
          signature = "FitDataMessage",
          function(object) {
              fieldDefinition(object@definition)
          }
)

setMethod("devFieldDefinition", 
          signature = "FitDefinitionMessage",
          function(object) {
            object@dev_field_defs
          }
)

setMethod("devFieldDefinition", 
          signature = "FitDataMessage",
          function(object) {
            devFieldDefinition(object@definition)
          }
)


.getValueForFieldNum <- function(object, value) {
    idx <- which(object@definition@field_defs$field_def_num == value)
    if(length(idx) == 0) { stop("Unkown field requested") }
    return(object@fields[[idx]])
}
