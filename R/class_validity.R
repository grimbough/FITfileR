validDevData <- function(object) {
    if(length(object@dev_fields) == nrow(object@definition@dev_field_defs)) TRUE
    else paste0("Slots 'dev_fields' and 'dev_field_details' must be the same length: ", 
               length(object@dev_fields), " vs ",
               length(object@dev_field_details))
}
