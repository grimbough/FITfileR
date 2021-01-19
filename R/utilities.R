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
    if(localMessageNumber(def) == local_message_number) {
      return(def)
    }
  }
}

.matchDevDefinition <- function(msgs, dev_data_idx) {
  
  i <- dev_data_idx[1]
  ## which elements in the definition relate to definition number, name, and units?
  field_defs <- fieldDefinition(msgs[[i]])
  idx <- match(c(1, 3, 8), field_defs$field_def_num)
  def <- msgs[[i]]@fields[ idx ]
  if(length(dev_data_idx) > 1) {
    for(j in seq_along(dev_data_idx[-1])) {
      i <- dev_data_idx[j+1]
      field_defs <- fieldDefinition(msgs[[i]])
      idx <- match(c(1, 3, 8), field_defs$field_def_num)
      def <- Map(c, def, msgs[[i]]@fields[ idx ])
    } 
  }
  names(def) <- c("field_def_num", "field_name", "units")
  return(def)
}