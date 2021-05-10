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
  # Load any .bib file
  bib_files <- list.files(
    path = path,
    pattern = "*.bib",
    recursive = TRUE,
    full.names = TRUE
  )

  # Remove file marked for overwrite
  if (file.exists(file) & overwrite) file.remove(file)

  # Sanity check
  stopifnot(
    "Couldn't find any '*.bib' files" = (length(bib_files) > 0),
    "'file' already exists" = !file.exists(file)
  )

  # Remove duplicate entries
  single_bibentry <- unique(
    do.call(c, lapply(bib_files, bibtex::read.bib))
  )

  # Warn over duplicate keys
  if (length(single_bibentry) > length(unique(single_bibentry$key))) {
    dup_keys <- paste0(
      single_bibentry$key[duplicated(single_bibentry$key)],
      collapse = "\n- "
    )
    warning("Found duplicate keys:\n- ", dup_keys, call. = FALSE)
  }

  # Write to file and return silently
  bibtex::write.bib(single_bibentry, file = file, append = TRUE)
  invisible(single_bibentry)
}
