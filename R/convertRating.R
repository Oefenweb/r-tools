#' Function to transform rating into transformed rating
#'
#' @param theta Normal theta rating
#' @param beta All item ratings of the associated items
#' @return Transformed rating
transRating <- function(theta, beta) {
  return(10 * sum(exp(theta - beta) / (1 + exp(theta - beta))) / length(beta))
}

#' Rating Transformation Function
#'
#' @param rating Theta, transformed rating or Qscore as input rating
#' @param domainId Game domain id
#' @param type Type of input rating, it should be "rating" for a normal theta rating,
#' "transformed rating" if the input rating is a transformed rating and "Q-Score" if the input
#' rating is a Q-Score
#' @param database name of database
#' @param printResults should results be printed?
#' @return Converted versions of input rating
#' @importFrom utils tail
#' @export
convertRating <- function(rating,
                          domainId,
                          type = c("rating",
                                   "transformed rating",
                                   "Q-Score"),
                          database = "oefenweb_nl_app",
                          printResults = TRUE) {
  # database connection
  con <- oefenwebDatabase::connect(database)
  domains <- suppressWarnings(DBI::dbReadTable(con, "domains"))

  # return nothing if domain is not correct
  if (!domainId %in% domains$id) {
    warning("Domain id not correct.")
    return()
  }

  # get items with domain id input
  items <- suppressWarnings(DBI::dbGetQuery(con, paste0("SELECT *
                                          FROM `items`
                                          WHERE `status` = 1
                                           AND `domain_id` = ", domainId)))

  # get qscore for domin id input
  Qscore <- suppressWarnings(DBI::dbGetQuery(con, paste0("SELECT *
                                          FROM `q_scores`
                                          WHERE `domain_id` = ", domainId)))

  # if rating is character, transform it to numeric
  if (is.character(rating)) {
    rating <- as.numeric(gsub("\\,", ".", rating))
  }

  # test function arguments
  type <- match.arg(type)

  # make transformation for type rating
  if (type == "rating") {
    Qscore$diff <- Qscore$rating - rating
    Qscore$diff[Qscore$diff < 0] <- 100

    qScore <- Qscore$q_score[which.min(Qscore$diff)]
    theta <- rating

    transformedRating <- transRating(theta, items$rating)
  }

  # make transformation for type transform ratings
  if (type == "transformed rating") {
    # get all user ratings
    ratings <- suppressWarnings(DBI::dbGetQuery(con, paste0("SELECT rating
                                          FROM `user_domain_ratings`
                                          LEFT JOIN `users`
                                            ON `user_domain_ratings`.`user_id` = `users`.`id`
                                          WHERE `users`.`role_id` = 1
                                           AND `users`.`active` = 1
                                           AND `user_domain_ratings`.`domain_id` = ",
                                                domainId)))
    # create transformed rating table
    mappingDF <- ratings
    # get transformed rating for all possible values
    mappingDF$transRating <- apply(mappingDF, 1, function(x) transRating(x[1], items$rating))

    transformedRating <- rating
    theta <- mappingDF$rating[which.min(abs(mappingDF$transRating - rating))]

    Qscore$diff <- Qscore$rating - theta
    Qscore$diff[Qscore$diff < 0] <- 100
    if (all(Qscore$diff >= 100)) {
      qScore <- tail(Qscore, 1)$q_score
    } else {
      qScore <- Qscore$q_score[which.min(Qscore$diff)]
    }
  }

  # make transformation for type qscore
  if (type == "Q-Score") {
    thetaIndex <- which.min(abs(Qscore$q_score - rating))
    thetaIndex <- ifelse(thetaIndex == 1000, 999, thetaIndex)
    theta <- Qscore$rating[thetaIndex]
    qScore <- rating
    transformedRating <- transRating(theta, items$rating)
  }

  # print Results
  if (printResults) {
    cat("\nRating:", round(theta, 3), "\n")
    cat("Transformed Rating:", round(transformedRating, 3), "\n")
    cat("Q-Score:", qScore, "\n\n")
  }

  # close connection
  invisible(oefenwebDatabase::close_connection(con))
  return(list(rating = round(theta, 3),
              transformedRating = round(transformedRating, 3),
              qScore = qScore))
}
