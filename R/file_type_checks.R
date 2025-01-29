#' Extract the type of FIT file
#'
#' Quickly determine what type of FIT file you have without decoding all the
#' data.  Typically types include 'activity', 'workout' and 'course'.  The full
#' range of options is defined in the FIT file specification.
#'
#' @param fileName A character specifying the FIT file to be read.
#'
#' @return As character vector of length 1.  This will contain the text
#'   representation of the file type e.g. 'activity'
#'
#' @examples
#' garmin_file <- system.file("extdata", "Activities", "garmin-edge530-ride.fit",
#'                            package = "FITfileR")
#' garmin <- readFitFileType(garmin_file)
#'
#' @export
readFitFileType <- function(fileName) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- .readFileHeader(con)

  msgDefs <- list()
  prev_header <- NULL
  
  while(seek(con, where = NA) < (file_header$data_size + file_header$size)) {
    
    record_header <- .readRecordHeader(con, prev_header)
    
    if(isDefinition(record_header)) {
      def <- .readMessage_definition(con = con, message_header = record_header)
      msgDefs[[ as.character(localMessageNumber(def)) ]] <- def
    } else {
      local_message_number = localMessageNumber_header(record_header)
      definition <- msgDefs[[ as.character(local_message_number) ]]
      
      message <- .readMessage_data(con = con, 
                                    header = record_header, 
                                    definition = definition)
      
      ## find the type and report as a string
      if(globalMessageNumber(message) == 0) {
        ## the type value is field definition 0
        idx <- which(fieldDefinition(message)$field_def_num == 0)
        fileTypeId <- message@fields[[idx]]
        
        fileTypeText <- .getFromDataTypeFactor(as.integer(fileTypeId), 'file')
        return(fileTypeText)
      }
      
    }
  }
}