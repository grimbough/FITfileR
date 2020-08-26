# 
# ## Writes the fit file header.
# .writeFileHeader <- function(con, original_header) {
#     
#     #header$size
#     writeBin(14L, con = con, size = 1)
#     #header$protocol_version
#     writeBin(original_header$protocol_version, con = con, size = 1)
#     #header$profile_version
#     writeBin(original_header$profile_version, con = con, size = 2, endian = "little")
#     #header$data_size
#     #data_size <- .calculateDataSize()
#     data_size <- 0
#     writeBin(data_size, con = con, size = 4, endian = "little")
#     #header$data_type
#     writeBin(charToRaw(".FIT"), con = con)
#     #header$crc <- readBin(con = con, what = "int", n = 2, size = 1)
#     writeBin(0L, con = con, size = 2)
#     #return(header)
# }
# 
# 
# .writeMessageHeader_normal <- function(con, definition_message = TRUE, local_message_type) {
#     
#     if(local_message_type > 15L | local_message_type < 0) {
#         stop("Invalid local message type: ", local_message_type)
#     }
#     
#     normal_header <- TRUE ## compressed time is FALSE
#     definition_message <- definition_message
#     developer_data <- FALSE
#     reserved_bit <- FALSE
#     lmt <- as.logical(intToBits(local_message_type)[1:4])
#     header_as_int <- fitFileR:::.binaryToInt( 
#         c(lmt, reserved_bit, developer_data, definition_message, normal_header) 
#     )
#     
#     writeBin(header_as_int, con = con, size = 1)
#     
# }