#' #' @export
#' setGeneric("writeFitFile", function(object) {
#'     standardGeneric("writeFitFile")
#' })
#' 
#' setMethod("writeFitFile", signature = "RawFitFile", function(object) {
#'     
#'     file_name <- "/tmp/test.fit"
#'     if(file.exists(file_name)) { file.remove(file_name) }
#'     
#'     con = file(file_name, open = "wb")
#'     on.exit(close(con))
#'     
#'     .writeFileHeader(con, original_header = object@header)
#'     
#' })