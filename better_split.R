better_split <- function(input, split){
  
  list_out <- strsplit(input, split)
  df_out <- do.call("rbind", list_out)
  
  colnames(df_out) <- c("names", "locations")
  ages <- regmatches(input, regexpr(split, input)) 
  df_out <- data.frame(df_out, ages)
  
  df_out$states <- substr(df_out$locations, 
                        nchar(df_out$locations)-1,
                        nchar(df_out$locations))
  
  df_out$cities<-apply(df_out, 1, function(x) {
    sub(x["states"], "", x["locations"])
  }
  )
  
  df_out <- df_out[,c("names", "cities", "states", "ages")]
  
  return(df_out)
  
  }
