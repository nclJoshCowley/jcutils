#' Collate BibTeX Databases
#'
#' Simple function that will find all .bib files in a directory and combine them
#'   into a single `BibEntry` object.
#'
#' @param path character. The directory to search recursively for .bib files.
#'
#' @seealso \code{\link[RefManageR]{ReadBib}}.
#'
#' @return \code{\link[RefManageR]{BibEntry}}.
#'
#' @export
collate_bibtex <- function(path = ".") {
  bib_paths <-
    list.files(
      path = path,
      pattern = "*.bib",
      recursive = TRUE,
      full.names = TRUE
    )

  stopifnot("Couldn't find any '*.bib' files" = (length(bib_paths) > 0))

  bib_list <- lapply(bib_paths, RefManageR::ReadBib)
  bib_list_keys <- lapply(bib_list, function(.db) vapply(.db, names, character(1)))

  bibs <- do.call(c, bib_list)
  bibs_keys <- do.call(c, bib_list_keys)

  if (anyDuplicated(bibs_keys)) {
    warning(
      "Found duplicate keys:\n- ",
      paste0(bibs_keys[duplicated(bibs_keys)], collapse = "\n- "),
      call. = FALSE
    )
  }

  return(bibs)
}
