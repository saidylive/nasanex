download_NEX <- function(variable, model, case, year) { # nolint: object_name_linter.
  check_meta_files()
  has_model <- is_nex_model_exist(model)
  has_case <- is_nex_case_exist(case)
  has_variable <- is_nex_variable_exist(variable)
  is_valid_year <- year >= 1950 & year <= 2100
  is_historical <- year >= 1950 & year <= 2014
  is_valid_historical <- ifelse(is_historical & case != "historical", FALSE, TRUE)
  model_hash <- get_nex_hash_by_model(model)

  if(!is_valid_year){message("Case shoulde be  should be between 1950 to 2100")}
  if(!is_valid_historical){message("Year should be between 'historical' for the year between 1950 to 2014")}

  if(has_model & has_case & has_variable & is_valid_year & is_valid_historical ){
    download_dir = file.path(getwd(), model, case, variable)
    if(!dir.exists(download_dir)){
      dir.create(download_dir, recursive = TRUE)
    }
    filename <- paste0(variable, "_day_", model, "_", case, "_", model_hash, "_gn_",
                       year, ".nc")
    filepath <- file.path(download_dir, filename)
    url <- paste0("https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6/",
                  model, "/", case, "/", model_hash, "/", variable, "/",
                  filename)
    if (!file.exists(filepath)) {
      download.file(url, filepath, quiet = F, method = "auto")
    }
  }
  return(T)
}
download_NEX("pr", "CESM2", "ssp245", 2024)

check_meta_files <- function() {
  destdir <- file.path("./data")
  filepath_final <- file.path(destdir, "files-list.csv")
  if (!file.exists(filepath_final)) {
    message("Downloading files metadata")
    downloadNEXFilesList()
  }
}

is_nex_model_exist <- function(model) {
  destdir <- file.path("./data")
  filepath.model <- file.path(destdir, "models.csv")
  models = read.csv(filepath.model)[[1]]
  if (!any(models ==  model)){
    message("model should be one from the list below: ")
    print(models)
    return(FALSE)
  }
  return(TRUE)
}
# is_nex_model_exist("ACCESS-ESM1-5")

is_nex_case_exist <- function(case) {
  destdir <- file.path("./data")
  filepath.case <- file.path(destdir, "case.csv")
  items = read.csv(filepath.case)[[1]]
  if (!any(items ==  case)){
    message("Case should be one from the list below: ")
    print(items)
    return(FALSE)
  }
  return(TRUE)
}
# is_nex_case_exist("dsaf")

is_nex_variable_exist <- function(variable) {
  destdir <- file.path("./data")
  filepath.variable <- file.path(destdir, "variable.csv")
  items = read.csv(filepath.variable)[[1]]
  if (!any(items ==  variable)){
    message("Variable should be one from the list below: ")
    print(items)
    return(FALSE)
  }
  return(TRUE)
}
# is_nex_variable_exist("tasm")

get_nex_hash_by_model <- function(model){
  destdir <- file.path("./data")
  filepath.model.hash <- file.path(destdir, "model-hash.csv")
  dfmh = read.csv(filepath.model.hash)
  hash <- dfmh$hash[dfmh$model == model]
  if(length(hash) == 0){
    return(FALSE)
  }
  return(hash)
}
# get_nex_hash_by_model("sadfs")
