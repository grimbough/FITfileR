#' @export
setGeneric("records", function(object) {
    standardGeneric("records")
})

setMethod("records", signature = "FitFile", function(object) {
    object@records@messages
})

#' @export
setGeneric("laps", function(object) {
    standardGeneric("laps")
})

setMethod("laps", signature = "FitFile", function(object) {
    object@laps@messages
})