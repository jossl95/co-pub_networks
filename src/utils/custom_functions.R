.load_quarto_dependencies = function(){
  library(knitr)
  library(rmarkdown)
}

# this function is deprecated for compatibility with the renv package

# fpackage.check <- function(packages) {
#   # Check for installed packages and install any that are missing
#   lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE)
#     library(x, character.only = TRUE)
#   }
#   })
# }

fpackage.check <- function(packages) {
  lifecycle::deprecate_warn(
    when = "2025-10-27",
    what = "fpackage.check()",
    details = paste0("Use renv for package installation and library()",
                     " for loading packages.")
  )
  message(
    "⚠️ fpackage.check() is deprecated. Use renv + library() calls instead."
  )
  
  invisible(TRUE)
}

fsave <- function(x, file, location = "./data/processed/", ...) {
  # if directory does not exist, create it
  if (!dir.exists(location)) dir.create(location)
  
  # create filename with date
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, sep = "")

  # save file
  print(paste("SAVING: ", totalname, sep = ""))
  save(x, file = totalname)
}

fsaveRDS <- function(x, file, location = "./data/processed/", ...) {
  # if directory does not exist, create it
  if (!dir.exists(location)) dir.create(location)
  
  # create filename with date
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, ".Rds", sep = "")

  # save file
  print(paste("SAVING: ", totalname, sep = ""))
  saveRDS(x, file = totalname)
}

freadRDS = function(fileName){
  # load file and return object
  readRDS(fileName)
}

freadRDS2 = function(file, location = "processed", ...){
  # make sure that the location is nested within the data directory
  if (!str_detect(file, '.Rds')) file = paste0(file, ".Rds")

  # format location and create the location if it does not exist
  if (!str_detect(location, 'data/')) location = file.path('data', location)
  if (!file.exists(location)) warning("the location does not exist ", location)

  # list and filter by literal substring 'file'
  files = list.files(location, pattern = file)
  files = files[grepl(file, files, fixed = TRUE)]

  # exclude old_* unless the requested file itself starts with old_
  if (!startsWith(file, "old_")) files = files[!startsWith(files, "old_")] 
  if (startsWith(file, "old_")) files = files[startsWith(files, "old_")] 

  # create file_path for the most recent file
  file_name = sort(files, decreasing = TRUE, na.last = NA)[1]
  file_path = file.path(location, file_name)

  return(readRDS(file_path))
}

fload <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}


normalize_name = function(x) {
  x = as.character(x)

  # mark Cyrillic
  has_cyr = stri_detect_charclass(x, "\\p{Script=Cyrillic}")

  # transliterate only Cyrillic -> Latin
  x[has_cyr] = stri_trans_general(x[has_cyr], "Cyrillic-Latin")

  # strip accents for all, lowercase, trim, squish
  x = stri_trans_general(x, "Latin-ASCII")
  x = tolower(x)
  x = str_squish(x)
  trimws(x)
}
# colorize <- function(x, color) {sprintf("<span style='color: %s;'>%s</span>", color, x) }

eval_ok <- tryCatch(isTRUE(params$eval), error = function(...) {
  tryCatch(isTRUE(param$eval), error = function(...) FALSE)
})