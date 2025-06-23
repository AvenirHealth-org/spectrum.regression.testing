#!/usr/bin/env Rscript

root <- here::here()

message("This repo requires external data from dropbox to run.")
message("The folder can be found here https://www.dropbox.com/scl/fo/7s2euzmaochus4gn1m6fx/AHCiBYBtAOlgbO644mNdFh4?rlkey=xae0wwt1h7qumxh3i8cx129c0&st=smwhkw95&dl=0")
message(paste("Copy or sync the 'Shiny regression testing' 'extracts'",
              "folder onto your local machine and enter the path below."))
message("Path will be interpreted as a string, there is no need to escape spaces.")
cat("Local path to extracts dir: ")
extract_path <- readLines("stdin", n = 1)

if (!file.exists(extract_path)) {
  ## We've probably not synced correctly
  stop(sprintf("Failed to locate path '%s', is there a typo? Check path exists and try again", extract_path))
}

if (!file.exists(file.path(extract_path, "aim2024"))) {
  ## We've probably not synced the correct folder
  stop(sprintf("Failed to locate 'aim2024' folder in dir at path '%s', have you synced the correct directory?", extract_path))
}

message("This repo also uses external data from spectrum-orderly orderly instance.")
message("Sync the remote repo from sharepoint https://futuresinstitute.sharepoint.com/:f:/s/Programming/Es57cTFvF_tKv0KzTKacj_sBaCtvQKke_UtfB8_dzE-LzQ?e=65bdZw and enter the local path below")
cat("Local path to orderly repo: ")
orderly_root <- readLines("stdin", n = 1)

if (!file.exists(orderly_root)) {
  ## We've probably not synced correctly
  stop(sprintf("Failed to locate path '%s', is there a typo? Check path exists and try again", orderly_root))
}

if (!file.exists(file.path(orderly_root, "orderly_config.yml"))) {
  ## We've probably not synced the correct folder
  stop(sprintf("Path '%s' exists, but can't locate orderly_config.yml. Is this an orderly root?", orderly_root))
}

writeLines(
  c(
    paste0("EXTRACTS_DIR=", extract_path),
    paste0("ORDERLY_ROOT=", orderly_root)
  ), file.path(root, ".env")
)
