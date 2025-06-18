#!/usr/bin/env Rscript

root <- here::here()

message("This repo requires external data from dropbox to run.")
message("The folder can be found here https://www.dropbox.com/scl/fo/7s2euzmaochus4gn1m6fx/AHCiBYBtAOlgbO644mNdFh4?rlkey=xae0wwt1h7qumxh3i8cx129c0&st=smwhkw95&dl=0")
message(paste("Copy or sync the 'Shiny regression testing' 'extracts'",
              "folder onto your local machine and enter the path below."))
message("Path will be interpreted as a string, there is no need to escape spaces.")
cat("Local path to extracts dir: ")
path <- readLines("stdin", n = 1)

if (!file.exists(path)) {
  ## We've probably not synced correctly
  stop(sprintf("Failed to locate path '%s', is there a typo? Check path exists and try again", path))
}

if (!file.exists(file.path(path, "aim2024"))) {
  ## We've probably not synced the correct folder
  stop(sprintf("Failed to locate 'aim2024' folder in dir at path '%s', have you synced the correct directory?", path))
}

writeLines(paste0("EXTRACTS_DIR=", path), file.path(root, ".env"))
