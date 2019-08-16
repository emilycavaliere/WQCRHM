#' Calculate inflow/outflow
#'
#' @param df dataframe merged from pond sizes, in, out and storage dataframe
#'@import tidyverse
#' @return f.df dataframe with calculated inflow, out, and outflow
#' @export
#'
#' @examples \dontrun
wbcalc<-function(df){
  outflow<-cbind(df$sr,df$sgw,df$ssr,df$et)
  df$outflow = rowSums(outflow)
  inflow<-cbind(df$mr, df$mi, df$rr, df$ri, df$condense,df$isd,df$isr, df$rre)
  df$inflow = rowSums(inflow)
  df$out=df$sr
df$out.rre<-as.numeric(df$out-df$rre)#Main outflow variable from a wetland is soil runoff ( ), but a portion of this returns back as  redirected residual
f.df<-df
return(f.df)
}
