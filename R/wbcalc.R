#' Calculate inflow/outflow
#'
#' @param df dataframe merged from pond sizes, in, out and storage dataframe
#'@import tidyverse
#' @return f.df dataframe with calculated inflow, out, and outflow
#' @export
#'
#' @examples \dontrun
wbcalc<-function(df){

f.df<-df%>%
  group_by(date, hru) %>%
  mutate(inflow = rowSums(cbind(mr, mi, rr, ri, condense,isd,isr), na.rm = T),
         out = rowSums(cbind(sr), na.rm = T),
         outflow = rowSums(cbind(sr,sgw,ssr,et), na.rm = T)#total outflow, would only use sr for my calcs
  )

f.df$outflow<-as.numeric(f.df$out-f.df$rre)#Main outflow variable from a wetland is soil runoff ( ), but a portion of this returns back as  redirected residual
return(f.df)
}
