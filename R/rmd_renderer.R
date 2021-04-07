#' R Markdown Renderer
#'
#' Given some report name, will create a function that calls `rmarkdown::render`
#'   with consistent input and output arguments. This allows for the same report
#'   to be compiled multiple times with slightly different arguments.
#'
#' @param name character scalar. This defines the report name that is required
#'   for consistency. See File Structure for details.
#' @param reports_dir directory string.
#'
#' @section File Structure:
#' All reports must be placed in their own folder (of the same name) within
#'   "inst/reports". For instance, if a report called "example" is created
#'   and saved as `inst/reports/example/example.Rmd` as below, the output
#'   folder (also shown) can be created by running the code in the examples
#'   section.
#'
#' ```
#'   +-- inst/
#'   |   +-- reports/
#'   |   |   +-- example/
#'   |   |   |   +-- example.Rmd
#'   |   |   |   +-- output/
#'   |   |   |   |   +-- example-subname1.pdf
#'   |   |   |   |   +-- example-subname2.pdf
#'   |   |   |   |   +-- example_subname3.html
#' ```
#'
#' @examples
#' \dontrun{
#' example_func <- rmd_renderer("example")
#' example_func(subname = "subname1")
#' example_func(subname = "subname2", params = list(seed = 2))
#' example_func(subname = "subname1", output_format = "html_document")
#' }
#'
#' @return A function that will render reports in the `output/` directory.
#'   The parameters of this sub-function include `subname` and `...`, the latter
#'   of which will be passed to `rmarkdown::render`.
#'
#' @export
rmd_renderer <- function(name, report_dir) {
  force(name)

  # Expect reports to be in following dir
  if (missing(report_dir)) {
    rep_dir <- here::here("inst/reports")
  } else {
    rep_dir <- report_dir %>%
      gsub(pattern = "\\\\", replacement = "/") %>%
      gsub(pattern = "/$", replacement = "")
  }
  if (!dir.exists(rep_dir)) stop("Couldn't find directory:\n ", rep_dir)

  # Define input Rmd file
  input <- paste0(rep_dir, "/", name, "/", name, ".Rmd")
  if (!file.exists(input)) stop("Couldn't find file, should be at ", input)

  # Define output directory
  output_dir <- paste0(rep_dir, "/", name, "/output")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # Return as function
  function(subname, ...) {
    stopifnot("subname should be a character" = is.character(subname))
    rmarkdown::render(
      input = input,
      output_file = paste0(output_dir, "/", name, "_", subname),
      output_dir = output_dir,
      ...
    )
  }
}
