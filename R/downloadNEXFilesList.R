library("stringr")
downloadNEXFilesList <- function() { # nolint
  defaultTimeout <- getOption("timeout")
  options(timeout = 100000000)
  url <- "https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/gddp-cmip6-files.csv" # nolint: line_length_linter.
  destdir <- file.path("./data")
  filepath <- file.path(destdir, "gddp-cmip6-files.csv")
  filepath.final <- file.path(destdir, "files-list.csv")
  filepath.model <- file.path(destdir, "models.csv")
  filepath.variable <- file.path(destdir, "variable.csv")
  filepath.case <- file.path(destdir, "case.csv")
  filepath.model.hash <- file.path(destdir, "model-hash.csv")

  if (!file.exists(filepath.final)) {
    download.file(url, filepath, quiet = F, method = "auto")
    options(timeout = defaultTimeout)
    df <- read.csv(filepath)
    paths <- str_replace(df$fileURL, "https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6/", "") # nolint: line_length_linter.
    path.parts <- strsplit(paths, "/")
    dfcols <- as.data.frame(do.call(rbind, path.parts))
    names(dfcols) <- c("model", "case", "hash", "variable", "file")
    dfcols$model <- str_trim(dfcols$model)
    dfcols$year <- as.numeric(str_sub(dfcols$file, -7, -4))
    dfcols$fileURL <- df$fileURL
    write.csv(dfcols, filepath.final, row.names = F)
    write.csv(data.frame(model = names(table(dfcols$model))), filepath.model,
      row.names = F
    )
    write.csv(data.frame(variable = names(table(dfcols$variable))), filepath.variable,
      row.names = F
    )
    write.csv(data.frame(case = names(table(dfcols$case))), filepath.case,
      row.names = F
    )
    write.csv(aggregate(file ~ model + hash, data = dfcols, FUN = "length"), filepath.model.hash,
      row.names = F
    )
  }
  return(url)
}