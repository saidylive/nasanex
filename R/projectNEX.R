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
  filepath = get_nex_nc_file_path(variable, model, case, year)
  download_dir = dirname(filepath)

  if(!dir.exists(download_dir) | !file.exists(filepath)){
    download_NEX(variable, model, case, year)
  }
  st <- stack(filepath, varname = variable)
  # stmean <- calc(st, fun = mean, na.rm = T)
  values <- extract(st, targetPoints)
  return(values)
}

#' Visualize NEX data for certain shape files
#'
#' Download NEX satelliste image based on variable, model, case and year in working directory
#' @param referenceShp Reference shape file object that want to visualize
#' @param variable name ex. tas, tasmax, tasmin, pr
#' @param model Model name ex. ACCESS-ESM1-5, CESM2
#' @param case Case name ex. ssp245, ssp585
#' @param year target year after 2015
#' @param fun Target Function that you want to aggregate. Default mean
#' @return Void
#' @examples
#' data(bdDistrictShapes)
#' visualize_NEX(bdDistrictShapes, "tas", "ACCESS-ESM1-5", "ssp245", 2028);
#' visualize_NEX(bdDistrictShapes, get_nex_variable_list()[1], get_nex_model_list()[1], get_nex_case_list()[1], 2028);
#' @export
visualize_NEX <- function(referenceShp, variable, model, case, year, fun = mean) {
  filepath = get_nex_nc_file_path(variable, model, case, year)
  download_dir = dirname(filepath)

  if(!dir.exists(download_dir) | !file.exists(filepath)){
    download_NEX(variable, model, case, year)
  }
  ex <- extent(referenceShp)
  e <- as(ex, 'SpatialPolygons')
  crs(e) <- crs(referenceShp)

  st <- stack(filepath, varname = variable)
  r <- crop(st, referenceShp)
  rmean <- calc(r, fun = fun, na.rm = T) - 273

  lplot = levelplot(rmean, margin = FALSE, contour=F,
            panel=function(...) {
              panel.levelplot(...)
              sp.polygons(referenceShp, fill = NA, col = "black", alpha=1)
            })

  # plot(rmean)
  # plot(referenceShp, add=T)
  return(list(data=rmean, fig=lplot))
}


#' Extract GRID points provided Shape file
#'
#' Download NEX satelliste image based on variable, model, case and year in working directory
#' @param referenceShp Reference shape that want to extract
#' @param variable name ex. tas, tasmax, tasmin, pr
#' @param model Model name ex. ACCESS-ESM1-5, CESM2
#' @param case Case name ex. ssp245, ssp585
#' @param year target year after 2015
#' @return Dataframe of all the points contains reference Shape Object in SP SpatialPointsDataFrame
#' @examples
#' data(bdDistrictShapes)
#' extract_grid_NEX(bdDistrictShapes, "tas", "ACCESS-ESM1-5", "ssp245", 2028);
#' extract_grid_NEX(bdDistrictShapes, get_nex_variable_list()[1], get_nex_model_list()[1], get_nex_case_list()[1], 2028);
#' @export
extract_grid_NEX <- function(referenceShp, variable, model, case, year) {
  filepath = get_nex_nc_file_path(variable, model, case, year)
  download_dir = dirname(filepath)
  if(!dir.exists(download_dir) | !file.exists(filepath)){
    download_NEX(variable, model, case, year)
  }
  ex <- extent(referenceShp)
  nc = nc_open(filepath)
  lats <- ncvar_get(nc, "lat")
  lons <- ncvar_get(nc, "lon")
  nc_close(nc)

  lats <- lats[lats >= ex[3] & lats <= ex[4]]
  lons <- lons[lons >= ex[1] & lons <= ex[2]]
  points.df <- expand.grid(lons, lats)
  points.df <- cbind(1:nrow(points.df), points.df)
  names(points.df) <- c("id", "lon", "lat")

  df <- as.data.frame(points.df)
  coordinates(df)= ~ lon+ lat
  crs(df) <- crs(referenceShp)
  o <- over(referenceShp, df)

  df <- df[df$id %in% o$id, ]

  # ap = plot(referenceShp)
  # plot(df, add=T, col="red")
  return(df)
}
