setMethod("length", 
          signature = c("FitFile"),
          function(x) {
            length(x@messages)
          }
)
