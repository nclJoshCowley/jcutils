#' Output Session Information
#'
#' A `knitr`-friendly output of \code{\link[utils]{sessionInfo}} with added git
#' information provided by `git2r`.
#'
#' @param git_repo character. Path to be passed to
#'   \code{\link[git2r]{repository_head}}.
#' @param params print output of non-NULL object will be captured and appended.
#'
#' @export
session_info <- function(git_repo, params = NULL) {
  if (missing(git_repo)) git_repo <- rprojroot::find_root("DESCRIPTION")

  si <- utils::sessionInfo()

  # Sort loaded packages alphabetically
  si$loadedOnly <- si$loadedOnly[order(names(si$loadedOnly))]

  as_bullets <- function(x) paste(sprintf("- `%s`", x), collapse = "\n")

  # Sections held in separate variables
  heading <-
    paste0(
      "- Executed: ", Sys.time(), "\n",
      "- ", si$R.version$version.string, "\n",
      "- Platform: ", si$platform, "\n",
      "- Running under: ", si$running, "\n",
      "- Matrix products: ", si$matprod
    )


  locale_body <- as_bullets(unlist(strsplit(si$locale, ";")))

  base_packages <- as_bullets(si$basePkgs)

  loaded_packages <-
    as_bullets(sprintf(
      "%s (v%s)",
      vapply(si$loadedOnly, function(.x) .x$Package, character(1)),
      vapply(si$loadedOnly, function(.x) .x$Version, character(1))
    ))

  # Git output is optional
  if (!is.null(git_repo)) {
    cur_branch <- git2r::repository_head(git_repo)
    cur_commit <- git2r::revparse_single(git_repo, revision = "HEAD")
    author <- cur_commit$author

    git_body <-
      paste0(
        "- Branch: ", sprintf("`%s`", cur_branch$name), "\n",
        "- Commit: ", sprintf("`%s`", git2r::sha(cur_commit)), "\n",
        "- Author: ", sprintf("%s (<%s>)", author$name, author$email), "\n",
        "- When: ", author$when, "\n",
        "- Summary: ", cur_commit$summary
      )
  }

  # params only shown when not NULL
  params_block <- if (is.null(params)) "" else {
      sprintf(
        "**Session parameters**\n\n```{r}\n%s\n```",
        paste0(capture.output(print(params)), collapse = "\n")
      )
  }

  # Knitr specific output / warning
  is_knitr <- isTRUE(getOption("knitr.in.progress"))

  if (is_knitr & knitr::opts_current$get("results") != "asis") {
    warning("Results = 'asis' is expected but not found.")
  }

  cat(
    # (Knitr)
    if (is_knitr) "\\newpage",
    if (is_knitr) "<details><summary>**Session information**</summary>",
    # (All)
    heading,
    "**Locale**", locale_body,
    "**Packages (base)**", base_packages,
    "**Packages (loaded)**", loaded_packages,
    "**Git**", git_body,
    params_block,
    # (Knitr)
    if (is_knitr) "</details>",
    sep = "\n\n"
  )

}
