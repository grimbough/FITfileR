# 
# 
# .writeMessage_definition <- function(con, global_message_number, field_definition) {
#     
#     ## write the 1-byte message header stating this is a definition message
#     .writeMessageHeader_normal(con = con, definition_message = TRUE)
#     
#     ## 5 bytes required for all messages
#     ## reserved
#     writeBin(0L, con = con, size = 1)
#     ## architecture - 0 = little endian
#     writeBin(0L, con = con, size = 1)
#     ## global message number
#     writeBin(global_message_number, con = con, size = 2, endian = "little")
#     ## number of fields in the data message
#     writeBin( as.integer(nrow(field_definition)), size = 1 )
#     
#     ## definition of data fields
#     for(i in seq_along(field_definition)) {
#         base_type <- field_definition[["base_type"]][i]
#         base_type_num <- which(names(data_type_lookup) == base_type)
#         if(length(base_type_num) == 0) {
#             stop("Error in determining base type number")
#         }
#         writeBin(
#             c(field_definition[["field_def_num"]][i],
#               field_definition[["size"]][i],
#               base_type_num),
#             con = con, size = 1
#         )
#     }
#     
# }
# 
# .writeMessage_data <- function(con, field_definition, messages) {
#     
#     ## iterate over rows
#     for(i in seq_along(messages)) {
#         ## iterate over each field
#         for(j in seq_len(ncol(messages))) {
#             base_type <- field_definition[["base_type"]][j]
#             data_type <- data_type_lookup[[base_type]]
#             val <- messages[[j]][i]
#             
#             if(base_type == "07") {
#                 writeBin(val, con = con, what = "character")
#             } else if(base_type %in% c("8c", "86")) { ## uint32
#                 writeBin( .uintToBits(messages[[j]][i]), con = con, size = 1 )
#             } else {
#                 writeBin(val, con = con, size = as.integer(data_type[[2]]), endian = "little")
#             }
#             
#         }
#         
#         
#     }
#     
# }
