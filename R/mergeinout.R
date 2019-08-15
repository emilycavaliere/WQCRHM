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

df<-df%>%
  group_by(date, hru) %>%
  mutate(
    condense=(condense/1000)*pond_area,
    isd=(isd/1000)*pond_area,
    isr=(isr/1000)*pond_area,
    ri=(ri/1000)*pond_area,
    mr=(mr/1000)*pond_area,
    rre=(rre/1000)*pond_area,
    rr=(rr/1000)*pond_area,
    mi=(mi/1000)*pond_area,
    gw=(gw/1000)*pond_area,
    et=(et/1000)*pond_area,
    sd=(sd/1000)*pond_area,
    sgw=(sgw/1000)*pond_area,
    sm=(sm/1000)*pond_area,
    scr=(scr/1000)*pond_area,
    sr=(sr/1000)*pond_area,
    ssr=(ssr/1000)*pond_area)


return(df)
}
