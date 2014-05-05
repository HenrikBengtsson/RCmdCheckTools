mkdirs <- function(path) {
  # Nothing todo?
  if (isTRUE(file.info(path)$isdir)) {
    return(TRUE);
  }
  dir.create(path, recursive=TRUE);
  # Sanity check
  stopifnot(isTRUE(file.info(path)$isdir));
  return(TRUE);
} # mkdirs()
