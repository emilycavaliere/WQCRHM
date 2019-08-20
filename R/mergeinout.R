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

#clean up:----
df2$date<-as.Date(df2$date, format='%Y-%m-%d')
df2$hru<-as.factor(df2$hru)
df<-df2[complete.cases(df2), ]

df$year<-year(df$date)
df$month<-month(df$date)

#fix units:----

df$condense=(df$condense/1000)*df$pond_area
df$isd=(df$isd/1000)*df$pond_area
df$isr=(df$isr/1000)*df$pond_area
df$ri=(df$ri/1000)*df$pond_area
df$mr=(df$mr/1000)*df$pond_area
df$rre=(df$rre/1000)*df$pond_area
df$rr=(df$rr/1000)*df$pond_area
df$mi=(df$mi/1000)*df$pond_area
df$gw=(df$gw/1000)*df$pond_area
df$et=(df$et/1000)*df$pond_area
df$sd=(df$sd/1000)
df$sgw=(df$sgw/1000)*df$pond_area
df$sm=(df$sm/1000)*df$pond_area
df$scr=(df$scr/1000)*df$pond_area
df$sr=(df$sr/1000)*df$pond_area
df$ssr=(df$ssr/1000)*df$pond_area



return(df)
}
