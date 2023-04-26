#' Extract NEX data for certain points
#'
#' Download NEX satelliste image based on variable, model, case and year in working directory
#' @param targetPoints Target points that want to extract in SpatialPointsDataFrame object
#' @param variable name ex. tas, tasmax, tasmin, pr
#' @param model Model name ex. ACCESS-ESM1-5, CESM2
#' @param case Case name ex. ssp245, ssp585
#' @param year target year after 2015
#' @return Void
#' @examples
#' data(bdDistrictCentroids)
#' extract_NEX(bdDistrictCentroids, "tas", "ACCESS-ESM1-5", "ssp245", 2028);
#' extract_NEX(bdDistrictCentroids, get_nex_variable_list()[1], get_nex_model_list()[1], get_nex_case_list()[1], 2028);
#' @export
extract_NEX <- function(targetPoints, variable, model, case, year) {
  dir_name <- "nex_nc"
  model_hash <- get_nex_hash_by_model(model)
  download_dir = file.path(getwd(), dir_name, model, case, variable)
  filename <- paste0(variable, "_day_", model, "_", case, "_", model_hash, "_gn_", year, ".nc")
  filepath <- file.path(download_dir, filename)
  if(!dir.exists(download_dir) | !file.exists(filepath)){
    download_NEX(variable, model, case, year)
  }
  st <- stack(nc_file_path, varname = variable)
  values <- extract(st, targetPoints)
  return(values)
}
