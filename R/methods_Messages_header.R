
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
