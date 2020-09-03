#' @param x An object of class \code{FitFile}.
#' @rdname FitFile-class
setMethod("length", 
          signature = c("FitFile"),
          function(x) {
            length(x@messages)
          }
)
