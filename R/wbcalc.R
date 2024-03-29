#' Calculate inflow/outflow
#'
#' @param df dataframe merged from pond sizes, in, out and storage dataframe
#'@import tidyverse
#' @return f.df dataframe with calculated inflow, out, and outflow
#' @export
#'
#' @examples \dontrun
wbcalc<-function(df){

  #df$outflow = rowSums(cbind(df$sr,df$sgw,df$ssr,df$et))
  df$outflow = as.numeric(df$sr+df$sgw+df$ssr+df$et)
  df$out.rre=as.numeric(df$sr-df$rre)#Main outflow variable from a wetland is soil runoff ( ), but a portion of this returns back as  redirected residual
  #df$inflow = rowSums(cbind(df$mr, df$mi, df$rr, df$ri, df$condense,df$isd,df$isr, df$rre), na.rm = T)
  df$inflow = as.numeric(df$mr+ df$mi+df$rr+ df$ri+df$condense+df$isd+df$isr+df$rre)
f.df<-df
return(f.df)
}


