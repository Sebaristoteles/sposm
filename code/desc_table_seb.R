desc_table <- function(x, y=NULL){
  if(!is.data.frame(x)){stop("Your input is not a data frame")}
  apply(x, 2,function(a){if(!is.vector(a)){stop("At least one column is not a vector")}})
  
  sapply(y, function(b){if(!is.function(b)){stop("The second argument is not a vector of functions")}})
  
  # test for numerical and logical columns
  check <- sapply(x, is.numeric) | sapply(x, is.logical)
  x <- x[, check]
  # flip columns and rows
  x_t = as.data.frame(t(x))
  
  fun_results <- x_t[NULL,]
  fun_results[, seq_along(funcs)] <- NA
  if (!missing(y))
  {
    for (c in seq_along(funcs))
    {
      fun_results[, c] <- apply(x_t, 1, function(c){y})
    }
    
    #assign(fun_results, apply(x_t, 1, function(c){y}), envir = globalenv() )
    #x_t_fun <- x_t[]
  }
  
  return(fun_result)
}

apply(df_t[1,], 1, function(d){lapply(funcs, function(e){e(d)})})
#fun_results <- df_t[, NULL]
# apply(df_t, 1, function(c){funcs(c)})
#assign(fun_results, apply(df_t, 1, function(c){funcs}), envir = globalenv() )
#desc_table(df, y = funcs)
#check <- sapply(df, is.numeric) | sapply(df, is.logical)
#df[, check]
#sum(sapply(df, is.numeric))

#df_t = as.data.frame(t(df))
