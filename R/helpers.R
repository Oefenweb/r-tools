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
                     camel_case_linter = NULL,
                     cyclocomp_linter = NULL,
                     seq_linter = NULL,
                     object_name_linter = NULL))

}

#' Function to create directories
#' @param mainDir Name of directory to create (in working directory)
#' @param subDir Name of sub directory that should be created
#' @return NULL (creates a directory)
#' @export
createDir <- function(mainDir, subDir = NULL) {
  # only check + create mainDir if subDir is not existent
  if (is.null(subDir)) {
    if (!dir.exists(file.path(getwd(), mainDir))) {
      dir.create(file.path(getwd(), mainDir), showWarnings = FALSE)
    }
  } else {
    # check + create sub and main dir
    if (!dir.exists(file.path(getwd(), mainDir, subDir))) {
      dir.create(file.path(getwd(), mainDir, subDir), showWarnings = FALSE)
    }
  }
}

#' Function to make first letter of string uppercase
#' @param x String
#' @return Input string with first letter capitalized
#' @export
UpperCase <- function(x) {
  return(sapply(x, function(x) paste(toupper(substring(strsplit(x, " ")[[1]], 1, 1)),
                                     substring(strsplit(x, " ")[[1]], 2),
                                     sep = "", collapse = " ")))
}

#' Function to extract all digits
#' @param x String
#' @return Numbers in in input string
#' @import readr
#' @export
ExtractDigits <- function(x) {
  return(readr::parse_number(x))
}
