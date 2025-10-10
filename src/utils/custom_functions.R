
clear_environment <- function() {
  # Clear the environment
  rm(list = ls())
  
  # Run garbage collection to free up memory
  gc()
}

fpackage.check <- function(packages) {
    # Check for installed packages and install any that are missing
    lapply(packages, FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    })
}

fsave <- function(x, file, location = "./data/processed/", ...) {
    # if directory does not exist, create it
    if (!dir.exists(location))
        dir.create(location)
    
    # create filename with date
    datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
    totalname <- paste(location, datename, file, sep = "")

    # save file
    print(paste("SAVING: ", totalname, sep = ""))
    save(x, file = totalname)
}

fload  <- function(fileName){
    # load file and return object
    load(fileName)
    get(ls()[ls() != "fileName"])
}

# colorize <- function(x, color) {sprintf("<span style='color: %s;'>%s</span>", color, x) }