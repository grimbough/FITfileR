.renameMessages <- function(scaffold, defs) {
  
  ## load the appropriate key/value table
  data("key_value.global_message", 
       package = "fitFileR", 
       envir = parent.frame())
  
  globalMessageNum <- sapply(defs, function(x) { x$global_message_num } )
  
  result <- vector("list", length = length(unique(globalMessageNum)))
  
  for(i in seq_along(result)) {
    gmn <- unique(globalMessageNum)[i]
    idx <- which(globalMessageNum == gmn)
    result[[ i ]] <- bind_rows(scaffold[idx])
    value_name <- filter(key_value.global_message, key == gmn) %>% 
      select(value) %>% 
      as.character()
    names(result)[i] <- value_name
  }
  return(result)
}

.processMessageType <- function(obj, name) {
  
  ## load the appropriate key/value table
  data(paste0("key_value.", name), 
       package = "fitFileR", 
       envir = parent.frame())
  
  kv.table <- eval(parse(text = paste0("key_value.", name)))
  current <- obj[[ name ]]
  idx <- match(names(current), kv.table[['key']])
  names(current) <- kv.table[['value']][idx]
  
  obj[[ name ]] <- current
  return(obj)
  
}