#' Processes Inflow Data from CRHM
#'
#' @param pht.in Name of unprocessed crhm inflow outputs
#'
#' @return temp.in that has been cleaned, reformated and ready to use to calculate water balance
#' @import tidyverse
#' @export
#'
#' @examples \dontrun
manipin <- function(pht.in) {
  #formatting dates:
  pht.in$Hours <- format(as.POSIXct(strptime(pht.in$datetime,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")
  pht.in$date <- as.Date(format(as.POSIXct(strptime(pht.in$datetime,"%Y-%m-%d %H:%M",tz="")) ,format = "%Y/%m/%d"))
  #from wide to long----

  dl.in <- gather(pht.in, name, measurement, meltrunoff.11:cum_hru_condense.56, factor_key=TRUE)

  #separate variable & hru----


  temp.pht.in<-dl.in %>%
    separate(name, c("variable", "hru"), sep = "[.]",  fill = "right", remove=T)
  head(temp.pht.in)

  #from long to wide (again)----

data_wide <- spread(temp.pht.in, key=variable, value=measurement, fill=NA)
  head(data_wide)
  summary(data_wide)
  pht.in<-data_wide

  #need to deaggregate from pht.in file:----

  pht.in$Sd<-pht.in$cum_to_Sd-lag(pht.in$cum_to_Sd, default = first(pht.in$cum_to_Sd))
  pht.in$Sd<-ifelse(pht.in$Sd<=0, 0, pht.in$Sd)

  pht.in$condense<-pht.in$cum_hru_condense-lag(pht.in$cum_hru_condense, default = first(pht.in$cum_hru_condense))

  pht.in$soil_rechr<-pht.in$cum_to_soil_rechr-lag(pht.in$cum_to_soil_rechr, default = first(pht.in$cum_to_soil_rechr))

  #Aggregate Daily values----

  #in values----
  names(pht.in)
  temp.in<-pht.in %>%
    group_by(hru, date) %>%
    summarize(
      condense=sum(condense),
      isd=sum(Sd),
      isr=sum(soil_rechr),
      ri=sum(infil),
      mr=sum(meltrunoff),
      rre=sum(redirected_residual),
      rr=sum(runoff),
      mi=sum(snowinfil)
    )

  return(temp.in)

}

