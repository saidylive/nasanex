#' NEX NC File download
#'
#' Download NEX satelliste image based on variable, model, case and year in working directory
#' @param variable name ex. tas, tasmax, tasmin, pr
#' @param model Model name ex. ACCESS-ESM1-5, CESM2
#' @param case Case name ex. ssp245, ssp585
#' @param year target year after 2015
#' @param force Force download if already downloaded before
#' @return Void
#' @examples
#' download_NEX("pr", "CESM2", "ssp245", 2025);
#' download_NEX("tas", "ACCESS-ESM1-5", "ssp245", 2028, force=T);
#' download_NEX("tasmax", "ACCESS-CM2", "ssp245", 2025);
#' download_NEX(get_nex_variable_list()[1], get_nex_model_list()[1], get_nex_case_list()[1], 2028);
#' @export
download_NEX <- function(variable, model, case, year, force=F) { # nolint: object_name_linter.
  check_meta_files()
  has_model <- is_nex_model_exist(model)
  has_case <- is_nex_case_exist(case)
  has_variable <- is_nex_variable_exist(variable)
  is_valid_year <- year >= 1950 & year <= 2100
  is_historical <- year >= 1950 & year <= 2014
  is_valid_historical <- ifelse(is_historical & case != "historical", FALSE, TRUE)
  model_hash <- get_nex_hash_by_model(model)
  dir_name <- "nex_nc"

  if(!is_valid_year){message("Case shoulde be  should be between 1950 to 2100")}
  if(!is_valid_historical){message("Year should be between 'historical' for the year between 1950 to 2014")}

  if(has_model & has_case & has_variable & is_valid_year & is_valid_historical ){
    filepath = get_nex_nc_file_path(variable, model, case, year)
    download_dir = dirname(filepath)
    filename = str_sub(filepath, str_length(download_dir)+2)
    if(!dir.exists(download_dir)){
      dir.create(download_dir, recursive = TRUE)
    }
    filename <- paste0(variable, "_day_", model, "_", case, "_", model_hash, "_gn_", year, ".nc")
    url <- paste0("https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6/",
                  model, "/", case, "/", model_hash, "/", variable, "/", filename)
    if (!file.exists(filepath) | force) {
      defaultTimeout <- getOption("timeout")
      options(timeout = 100000000)
      download.file(url, filepath, quiet = F, method = "curl")
      options(timeout = defaultTimeout)
    }
  }
}
# download_NEX("pr", "CESM2", "ssp245", 2024)

check_meta_files <- function() {
  destdir <- file.path("./nex_data")
  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = TRUE)
  }
  filepath_final <- file.path(destdir, "files-list.csv")
  if (!file.exists(filepath_final)) {
    message("Downloading files metadata")
    downloadNEXFilesList()
  }
}

#' NEX models list
#'
#' Get the list of all available models to download
#' @return list of array with model names
#' @examples
#' get_nex_model_list();
#' @export
get_nex_model_list <- function() {
  check_meta_files()
  destdir <- file.path("./nex_data")
  filepath.model <- file.path(destdir, "models.csv")
  models = read.csv(filepath.model)[[1]]
  return(models)
}
# get_nex_model_list()

is_nex_model_exist <- function(model) {
  models = get_nex_model_list()
  if (!any(models ==  model)){
    message("model should be one from the list below: ")
    message(cat(models, sep = "\n"))
    return(FALSE)
  }
  return(TRUE)
}
# is_nex_model_exist("ACCESS-ESM1-5")

#' NEX Case list
#'
#' Get the list of all available cases to download
#' @return list of array with model names
#' @examples
#' get_nex_case_list();
#' @export
get_nex_case_list <- function() {
  check_meta_files()
  destdir <- file.path("./nex_data")
  filepath.case <- file.path(destdir, "case.csv")
  items = read.csv(filepath.case)[[1]]
  return(items)
}

is_nex_case_exist <- function(case) {
  items = get_nex_case_list()
  if (!any(items ==  case)){
    message("Case should be one from the list below: ")
    message(cat(items, sep = "\n"))
    return(FALSE)
  }
  return(TRUE)
}
# is_nex_case_exist("dsaf")

#' NEX variable list
#'
#' Get the list of all available variables to download
#' @return list of array with variable names
#' @examples
#' get_nex_variable_list();
#' @export
get_nex_variable_list <- function() {
  check_meta_files()
  destdir <- file.path("./nex_data")
  filepath.variable <- file.path(destdir, "variable.csv")
  items = read.csv(filepath.variable)[[1]]
  return(items)
}

is_nex_variable_exist <- function(variable) {
  items = get_nex_variable_list()
  if (!any(items ==  variable)){
    message("Variable should be one from the list below: ")
    message(cat(items, sep = "\n"))
    return(FALSE)
  }
  return(TRUE)
}
# is_nex_variable_exist("tasm")

get_nex_hash_by_model <- function(model){
  destdir <- file.path("./nex_data")
  filepath.model.hash <- file.path(destdir, "model-hash.csv")
  dfmh = read.csv(filepath.model.hash)
  hash <- dfmh$hash[dfmh$model == model]
  if(length(hash) == 0){
    return(FALSE)
  }
  return(hash)
}
# get_nex_hash_by_model("sadfs")

get_nex_nc_file_path <- function(variable, model, case, year){
  dir_name <- "nex_nc"
  model_hash <- get_nex_hash_by_model(model)
  download_dir = file.path(getwd(), dir_name, model, case, variable)
  filename <- paste0(variable, "_day_", model, "_", case, "_", model_hash, "_gn_", year, ".nc")
  filepath <- file.path(download_dir, filename)
  return(filepath)
}

