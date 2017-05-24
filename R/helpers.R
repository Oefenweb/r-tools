#' Track progress of a for loop
#'
#' @param current Current iteration of for loop
#' @param max Maximum iteration of for loop
#' @return Prints out progress in percentage
#' @importFrom "utils" "flush.console"
#' @export
Progressor <- function(current, max) {
  cat("\r Completed: ", (round(current / max, 2) * 100), "%", sep = "")
  flush.console()
}

#' Lint script with Oefenweb lintr profile
#'
#' @param lintrPath Full path of to-be-linted file
#' @return Lintr output
#' @import lintr
#' @export
lintrProfile <- function(lintrPath) {
  rm(list = setdiff(ls(), "lintrPath"))
  lint(lintrPath,
       with_defaults(line_length_linter(120),
                     camel_case_linter = NULL))

}
