#' Reads parameters from CRHM .prj file
#'
#' @param prj_file Name of prj file
#'
#' @return Returns a list containing: HRU_name, HRU_SD_max, HRU_area
#' @import CRHMr
#' @export
#'
#' @examples \dontrun{HRU_params <- getHRUparameters("model.prj")}
getHRUparameters <- function(prj_file) {
  HRU_name <- readPrjHRUnames(prj_file)
  HRU_SD_max <- readPrjParameters(prj_file, "Sdmax")
  HRU_area <- readPrjParameters(prj_file, "hru_area")
  HRU_volume<-HRU_SD_max*HRU_area

  returned <- list(HRU_name = HRU_name, HRU_SD_max = HRU_SD_max, HRU_area = HRU_area)
  returned <- data.frame(HRU_name, HRU_SD_max, HRU_area, stringsAsFactors = FALSE)
  return(returned)
}
