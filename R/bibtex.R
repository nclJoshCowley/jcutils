#' Collate BibTeX Entries
#'
#' Simple function that will find all .bib files in a directory and combine them
#'   into a single .bib file.
#'
#' @param path character. The directory (and sub-directories) to search
#'   for .bib files
#' @inheritParams bibtex::write.bib
#' @param overwrite logical. If TRUE, replaces 'file', if it already exists.
#'
#' @return (invisible) An object of class "bibentry",
#'   containing all found references.
#'
#' @export
collate_bibtex <- function(path, file = "references.bib", overwrite = FALSE) {
  # Remove file marked for overwrite
  if (file.exists(file) & overwrite) unlink(file)
  stopifnot("'file' already exists" = !file.exists(file))

  # Load any .bib file
  bib_db <- data.frame(
    files = list.files(
      path = path,
      pattern = "*.bib",
      recursive = TRUE,
      full.names = TRUE
    )
  )
  stopifnot("Couldn't find any '*.bib' files" = (length(bib_db$files) > 0))

  # Parse bib entries as a warning system for invalid files
  withCallingHandlers(
    warning = function(cnd) {
      stop(
        "Couldn't parse .bib files for following reasons:\n",
        cnd,
        call. = FALSE
      )
    },
    bib_db$entry <- lapply(bib_db$files, bibtex::read.bib)
  )

  # Warn over duplicate keys
  all_keys <- unlist(lapply(bib_db$entry, function(x) x$key))
  if (anyDuplicated(all_keys)) {
    dup_keys <- all_keys %in% all_keys[duplicated(all_keys)]
    warning(
      "Found duplicated keys.\n ",
      paste0(all_keys[dup_keys], collapse = "\n "),
      call. = FALSE
    )
  }

  # To keep encoding and comments, join all files as text files
  raw_text <- paste0(
    unlist(lapply(
      bib_db$files,
      function(.x) paste0(readLines(.x, warn = FALSE), collapse = "\n")
    )),
    collapse = "\r\n\n"
  )

  writeLines(
    paste0(
      "% Generated using Collate-references.R. Do not edit by hand.\r\n\n",
      raw_text
    ),
    "references.bib"
  )
}






