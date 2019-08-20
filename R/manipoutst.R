
#' Processes Outflow and Storage Data from CRHM
#'
#' @param pht.outst Name of unprocessed crhm outflow and storage outputs
#'@import tidyverse
#' @return temp.outst that has been cleaned, reformated and ready to use to calculate water balance
#' @export
#'
#' @examples \dontrun
manipoutst<-function(pht.outst){
#Fix date in pht.in and pht.outst:----
pht.outst$datetime<-as.character(pht.outst$datetime)
pht.outst$Hours <- format(as.POSIXct(strptime(pht.outst$datetime,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")
pht.outst$date <- as.Date(format(as.POSIXct(strptime(pht.outst$datetime,"%Y-%m-%d %H:%M",tz="")) ,format = "%Y/%m/%d"))

#from wide to long----

dl.outst <- gather(pht.outst, name, measurement, hru_actet.11:gw.56, factor_key=TRUE)

#separate variable & hru----

temp.pht.outst<-dl.outst %>%
  separate(name, c("variable", "hru"), sep = "[.]",  fill = "right", remove=T)


#from long to wide (again)----

data_wide <- spread(temp.pht.outst, key=variable, value=measurement, fill=NA)
pht.outst<-data_wide

#Aggregate Daily values----
#out and storage----

temp.outst<-pht.outst %>%
  group_by(hru, date) %>%
  summarize(
    gw = mean(gw[Hours=="00:00"], na.rm = T),
    et=sum(hru_actet),
    sd = mean(Sd[Hours=="00:00"], na.rm = T),
    sgw=sum(soil_gw),
    sm=sum(soil_moist),
    scr=sum(soil_rechr),
    sr=sum(soil_runoff),
    ssr=sum(soil_ssr)
  )


return(temp.outst)
}
