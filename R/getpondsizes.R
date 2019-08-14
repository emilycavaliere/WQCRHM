#' Returns parabolic pond sizes: area, perimeter and depth
#'
#' @param CRHM_output_file has all the crhm stuff
#' @param returned extracted values for area and sd_max
#' @import  tidyverse
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

  return(ps.w)
}
