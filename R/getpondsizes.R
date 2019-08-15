#' Returns parabolic pond sizes: area, perimeter and depth
#'
#' @param CRHM_output_file has all the crhm stuff
#' @param returned extracted values for area and sd_max
#' @import  tidyverse CRHMr
#' @return Pond area, perimeter and depth on the parabola using CRHMr
#' @export
#'
#' @examples \dontrun
getpondsizes <- function(CRHM_output_file, returned) {
  CRHM_output <- readOutputFile(CRHM_output_file, timezone = "Etc/GMT+7")
  n <-ncol(CRHM_output);m <-n-1
  output_cols <- seq(1:m)
  CRHM_daily <- aggDataframe(CRHM_output, columns = output_cols, period = "daily", funs = "mean")
  pond_sizes <- wetland_pond_size(CRHM_output = CRHM_daily, HRU_name = returned$HRU_name, HRU_SD_max = returned$HRU_SD_max, HRU_area =  returned$HRU_area)
  data_long <- gather(pond_sizes, name, measurement, pond_area.1: pond_perimeter.46, factor_key=TRUE)
  temp.ps<-data_long %>%
    separate(name, c("variable", "hru"), sep = "[.]",  fill = "right", remove=F)
  #pond sizes back to wide:----
  ps.w <- spread(temp.ps, key=variable, value=measurement)
  summary(ps.w)
  head(ps.w)
  #group by date and hru
  ps.w <- ps.w %>%
    group_by(date, hru) %>%
    summarise(
      pond_area=mean(pond_area, na.rm = T),
      pond_depth=mean(pond_depth, na.rm = T),
      pond_perimeter=mean(pond_perimeter, na.rm = T))

  crhm_area<-data.frame(returned$HRU_name, returned$HRU_area)
  crhm_area <- crhm_area[11:56,]
  temp.crhmarea<-crhm_area %>%
    separate(returned.HRU_name, c("wetland", "hru"), sep = "Wetland",  fill = "right", remove=F)
  temp.crhmarea<- temp.crhmarea[c(3:4)]

sd<- CRHM_daily[c(1,70:115)]
data_long <- gather(sd, name, sd.mm, Sd.11.mean: Sd.56.mean, factor_key=TRUE)
temp.sd<-data_long %>%
  separate(name, c("variable", "hru","fun"), sep = "[.]",  fill = "right", remove=F)
temp.sd$hru<-as.numeric(temp.sd$hru)
temp.sd$hru<-as.numeric(temp.sd$hru-10)
sd<- temp.sd[c(1,4, 6)]
ps.w<-merge(ps.w, temp.crhmarea, by = "hru")
ps.w<-merge(ps.w, sd, by=c("date","hru"))
ps.w$crhm_area<-ps.w$returned.HRU_area*1000000
ps.w$sd.m<-ps.w$sd.mm/1000
  ps.w$volume<-ps.w$crhm_area*ps.w$sd.m
  return(ps.w)
}
