.binaryToInt <- function(bits) {
  int <- as.integer(sum(as.integer(bits) * 2^(seq_along(bits)-1)))
  return(int)
}

## convert double representing a uint32 into a raw vector
.uintToBits <- function(x) {
    
    if(x >= 0 && x < 2^31) {
        res <- intToBits(x)
    } else if(x >= 2^31 && x < 2^32) {
        x <- x-(2^31)
        res <- intToBits(x)
        res[32] <- as.raw(1)
    } else {
        stop("Unable to convert ", x, " to unit32. Outside range.")
    }
    return(res)
}

## create a signature for each definition message
## so we can easily separate message of the same type 
.definitionSignature <- function(field_defs) {
  paste(field_defs$field_def_num, 
        field_defs$size, 
        sep = "x",
        collapse = "_")
}

## for a given local message type, find the most recent definition message
## using that local message type.
.matchDefinition <- function(msgDefs, local_message_number) {
  for(i in rev(seq_along(msgDefs))) {
    def <- msgDefs[[i]]
    if(def@header@local_message_number == local_message_number) {
      return(def)
    }
  }
}

.matchDevDefinition <- function(msgs, dev_data_idx) {
  
  dev_msgs <- msgs
  i <- dev_data_idx
  #def <- field_def_num = dev_msgs[[i]]@fields['1']],
  #                  field_name = dev_msgs[[i]]@fields[['3']],
  #                  units = dev_msgs[[i]]@fields[['8']])
  def <- dev_msgs[[i]]@fields[ c('1','3','8') ]
  names(def) <- c("field_def_num", "field_name", "units")
  return(def)
}