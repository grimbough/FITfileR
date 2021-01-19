
setMethod("hasDeveloperData", 
          signature = c("FitDataMessage"),
          function(object) {
              return(hasDeveloperData(object@definition))
          }
)

setMethod("hasDeveloperData", 
          signature = c("FitDefinitionMessage"),
          function(object) {
              return(hasDeveloperData(object@header))
          }
)

setMethod("hasDeveloperData", 
          signature = c("FitMessageHeader"),
          function(object) {
              return(as.logical(object@raw_rep[6]))
          }
)



setMethod("isDefinition", 
          signature = c("FitMessageHeader"),
          function(object) {
              return(as.logical(object@raw_rep[7]))
          }
)

setMethod("timeOffset",
          signature = c("FitMessageHeader"),
          function(object) {
              if(object@is_compressed)
                  return(.binaryToInt(object@raw_rep[1:5]))
              else
                  return(0)
          }
)

