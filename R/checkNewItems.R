# initalize global variables for lintr
item_id <- correct_answered <- response_in_milliseconds <- rawEncAnswer <- avg_correct_answered <- NULL
per_questionmark <- per_overTime <- NULL
#' Function to get item ids based on creation date
#' @param creationDate date of item creation
#' @param domain_id id of domain
#' @return item_ids of relevant items
#' @import oefenwebDatabase
#' @import DBI
getItemIds <- function(creationDate, domain_id) {
  con <- oefenwebDatabase::connect()
  items <- suppressWarnings(DBI::dbGetQuery(con, paste0(
    "SELECT *
    FROM `items`
    WHERE `status` = 1 AND DATE(`created`) = '", creationDate, "' AND `domain_id` = ",
    domain_id)))
  oefenwebDatabase::close_connection(con)
  return(items$id)
}

#' Function to check if there are any problems with new items
#' @param item_ids ids of new items
#' @param creationDate date of item creation
#' @param domain_id id of domain
#' @return item summary if everythin is ok with new items
#' @import oefenwebDatabase
#' @import DBI
#' @import dplyr
#' @export
checkNewItems <- function(item_ids, creationDate = NULL, domain_id = NULL) {
  correctAnsweredThreshold <- 0.5
  questionThreshold <- 5
  overTimeThreshold <- 5
  con <- oefenwebDatabase::connect()
  if (missing(item_ids)) {
    if (is.null(creationDate) | is.null(domain_id)) {
      stop("If you are not supplying item ids, you need to fill in creationDate AND domain_id")
    }
    item_ids <- getItemIds(creationDate, domain_id)
    if (length(item_ids) == 0) {
      stop("No items found with this creationDate AND domain_id")
    }
  }
  items <- suppressWarnings(DBI::dbGetQuery(con, paste0(
    "SELECT *
    FROM `items`
    WHERE `id` IN (", paste(item_ids, collapse = ", "), ")")))
  if (nrow(items) == 0) {
    stop("No items found with input item id's")
  }
  parsedItems <- tryCatch({
    suppressMessages(oefenwebTools::getItems(unique(items$domain_id), itemStatus = c(1:10)))
  }
  , error = function(e) {
    message("error parsing items, just using unparsed items")
    data.frame()
  })
  # get log_records
  log_records <- suppressWarnings(DBI::dbGetQuery(con, paste0(
    "SELECT *
    FROM `log_records`
    WHERE `item_id` IN (", paste(item_ids, collapse = ", "), ")")))
  # transform answer because non-ascii character like question mark are not valid
  log_records$rawEncAnswer <- sapply(log_records$answer, function(x) paste(unlist(charToRaw(x)), collapse = " - "))
  # make item stats
  item_responses <- log_records %>%
    group_by(item_id) %>%
    summarise(avg_correct_answered = mean(correct_answered, na.rm = TRUE),
              avg_rt = mean(response_in_milliseconds / 1000, na.rm = TRUE),
              # question mark raw encoding
              per_questionmark = round( (sum(rawEncAnswer == "c2 - bf", na.rm = TRUE) / n()) * 100, 2),
              # deadline passed raw encoding
              per_overTime = round( (sum(rawEncAnswer == "e2 - 80 - a6", na.rm = TRUE) / n()) * 100, 2)) %>%
    as.data.frame()
  # if parsed items are available use them
  if (is.null(parsedItems)) {
    item_responses <- dplyr::left_join(item_responses, items, by = c("item_id" = "id"))
  } else {
    item_responses <- dplyr::left_join(item_responses, parsedItems, by = c("item_id" = "item_id"))
  }
  # correct answer too low
  correctLow <- subset(item_responses,
                       avg_correct_answered < correctAnsweredThreshold)
  # questionmark use too high
  questionmarkHigh <- subset(item_responses,
                             per_questionmark > questionThreshold)
  # passed deadline too high
  overTimeHigh <- subset(item_responses,
                         per_overTime > overTimeThreshold)
  # make summary
  itemSummary <- data.frame(check = c("Number of input item ids:",
                                      "Items created:",
                                      "item log_records:",
                                      "earliest log_record:",
                                      paste0("Correct Answer Proportion lower than ", correctAnsweredThreshold, ":"),
                                      paste0("Percentage Question Mark higher than ", questionThreshold, ":"),
                                      paste0("Percentage Passed Deadline higher than ", overTimeThreshold, ":")),
                            count = c(length(item_ids),
                                      paste(unique(items$created), collapse = " - "),
                                      nrow(log_records),
                                      as.character(min(as.Date(log_records$created))),
                                      nrow(correctLow),
                                      nrow(questionmarkHigh),
                                      nrow(overTimeHigh)),
                            listname = c("",
                                         "",
                                         "",
                                         "",
                                         "correctLow",
                                         "questionmarkHigh",
                                         "overTimeHigh"),
                            stringsAsFactors = FALSE)
  cat("\n\n#############\nItem Summary:\n")
  print(itemSummary)
  cat("\n#############\n")
  return(list(itemSummary = itemSummary,
              correctLow = correctLow,
              questionmarkHigh = questionmarkHigh,
              overTimeHigh = overTimeHigh))
  oefenwebDatabase::close_connection(con)
}
