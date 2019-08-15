#' merge pondsizes, tempin, tempoutst
#'
#' @param ps.w pond sizes
#' @param temp.in crhm inflow
#' @param temp.outst crhm outflow
#' @import tidyverse
#'
#' @return combined dataframe
#' @export
#'
#' @examples \dontrun
mergeinout<-function(ps.w, temp.in, temp.outst) {
  ##merge files----

  #first merge:----

df1<-merge(ps.w, temp.in, by = c("date","hru"), all = TRUE, sort = FALSE)
df1$date<-as.Date(df1$date, format='%Y-%m-%d')
df1$hru<-as.factor(df1$hru)

#second merge:----
df2=merge(df1, temp.outst, by = c("date","hru"), all = TRUE, sort = FALSE)

#final clean up:----
df2$date<-as.Date(df2$date, format='%Y-%m-%d')
df2$hru<-as.factor(df2$hru)
df<-df2[complete.cases(df2), ]

df$year<-year(df$date)
df$month<-month(df$date)


return(df)
}
