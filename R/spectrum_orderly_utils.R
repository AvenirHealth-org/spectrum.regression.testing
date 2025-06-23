list_spectrum_versions <- function(orderly_root) {
  task_name <- get_golem_config("orderly_task_name")
  task_ids <- orderly2::orderly_search(name = task_name, root = orderly_root)
  unique(unlist(lapply(task_ids, function(id) {
    meta <- orderly2::orderly_metadata(id, root = orderly_root)
    meta$parameters$spectrum_version
  })))
}

list_orderly_versions <- function(orderly_root, spectrum_version, leapfrog) {
  task_name <- get_golem_config("orderly_task_name")
  ## TODO: Add leapfrog run parameter to this
  orderly2::orderly_search(
    "parameter:spectrum_version == this:spectrum_version",
    name = task_name,
    parameters = list(spectrum_version = spectrum_version,
                      leapfrog = leapfrog),
    root = orderly_root)
}

get_orderly_file_paths <- function(orderly_root, orderly_id) {
  t <- tempfile()
  dir.create(t)
  orderly2::orderly_copy_files(orderly_id, c("aim_extract.ex", "out.xlsx"), t)
  list(
    extract_config = file.path(t, "aim_extract.ex"),
    extract = file.path(t, "out.xlsx")
  )
}
