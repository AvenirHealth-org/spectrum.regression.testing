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
  ids <- orderly2::orderly_search(
    "parameter:spectrum_version == this:spectrum_version && parameter:run_leapfrog == this:leapfrog",
    name = task_name,
    parameters = list(spectrum_version = spectrum_version,
                      leapfrog = leapfrog),
    root = orderly_root)

  display_name <- vapply(ids, function(id) {
    meta <- orderly2::orderly_metadata(id, root = orderly_root)
    display_date <- lubridate::ymd_hms(lubridate::round_date(meta$time$end, unit = "second"))
    git_hash <- meta$custom$orderly$description$custom$spectrum_git_hash
    orderly_id <- strsplit(id, "-")[[1]][3]
    if (is.null(meta$custom$orderly$description$custom$spectrum_git_hash)) {
      sprintf("%s %s", display_date, orderly_id)
    } else {
      sprintf("%s [%s] %s", display_date, git_hash, orderly_id)
    }
  }, character(1))
  setNames(ids, display_name)
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
