#' Function to obtain items without JSON format and with tags (if available).
#'
#' @param domainId An item domain id
#' @param itemStatus Item status (in numeric or character),
#'                   it is possible to enter several states like this
#'                   for example: c(1, 2, 3).
#' @param dbname Name of database
#' @return Item dataset with tags (if available) and without JSON formatting
#' @export
getItems <- function(domainId,
                     itemStatus = 1,
                     dbname = "oefenweb_nl_app") {
  con <- oefenwebDatabase::connect(dbname)
  domains <- suppressWarnings(DBI::dbReadTable(con, "domains"))
  domainId <- suppressWarnings(as.numeric(domainId))

  # check if domainId is valid
  if (is.na(domainId)) {
    message("The domain id must be a number object.")
    return(NULL)
  }
  if (!(domainId %in% domains$id)) {
    message("This item domain id does not exist.")
    return(NULL)
  }

  # make status string if several states are requested
  statusStr <- paste(itemStatus, collapse = ",")

  items <- suppressWarnings(DBI::dbGetQuery(con,
                                            paste0("SELECT *
                                                  FROM items
                                                  WHERE domain_id = ",
                                                  domainId,
                                                  " AND status IN (", statusStr, ")")))

  colnames(items)[which(colnames(items) == "id")] <- "item_id"
  itemIds <- items$item_id
  itemIdsPasted <- paste(itemIds, collapse =  ",")

  itemTags <- suppressWarnings(DBI::dbGetQuery(con,
                                               paste0(" SELECT *
                                                     FROM items_tags
                                                     WHERE item_id IN (",
                                                     itemIdsPasted, ")")))

  if (nrow(itemTags) != 0) {
    tagIdsPasted <- paste(itemTags$tag_id, collapse = ",")

    tags <- suppressWarnings(DBI::dbGetQuery(con,
                                             paste0("SELECt *
                                                   FROM tags
                                                   WHERE id IN (",
                                                   tagIdsPasted, ")")))
    colnames(tags)[which(colnames(tags) == "id")] <- "tag_id"

    tagNames <- plyr::join(itemTags, tags, by = "tag_id")[, c("item_id", "name")]

    maxTagNr <- max(table(tagNames$item_id))

    tagDF <- data.frame(matrix(NA, ncol = maxTagNr + 1, nrow = nrow(items)))
    colnames(tagDF) <- c("item_id", paste0("tag", c(1:maxTagNr)))

    for (i in 1:length(unique(tagNames$item_id))) {
      tagDF$item_id[i] <- unique(tagNames$item_id)[i]
      curTags <- tagNames$name[which(tagNames$item_id == unique(tagNames$item_id)[i])]
      tagDF[i, c(2:(length(curTags) + 1))] <- curTags
    }

    itemsWithTags <- plyr::join(items, tagDF, by = "item_id")
  } else {
    message(paste0("There are no tags for items with domain id ", domainId,
                   " (", domains$name[which(domains$id == domainId)], ")."))
    itemsWithTags <- items
  }
  # add learning goals
  itemLG <- suppressWarnings(DBI::dbGetQuery(con,
                                               paste0("SELECT `ilg`.`item_id`,
                                                              `ilg`.`learning_goal_id`,
                                                              `lg`.`name` AS `lg_name`,
                                                              `lg`.`description` AS `lg_description`,
                                                              `lg`.`grade` AS `lg_grade`,
                                                              `lg`.`position` AS `lg_position`,
                                                              `lg`.`show_in_results` AS `lg_show_in_results`,
                                                               `lg`.`is_playable` AS `lg_is_playable`
                                                       FROM `items_learning_goals` AS `ilg`
                                                       LEFT JOIN `learning_goals` AS `lg`
                                                       ON `ilg`.`learning_goal_id` = `lg`.`id`
                                                      WHERE `ilg`.`item_id` IN (",
                                                      itemIdsPasted, ")")))
  itemsWithTagsLG <- merge(x = itemsWithTags, y = itemLG, by = "item_id", all.x = TRUE)
  message(paste0("Pay attention.\n",
                 paste(strwrap("It's possible that there are multiple entries for an item if it is attached to
                               multiple learning goals.", 50), collapse = "\n")))
  # close connnection
  oefenwebDatabase::close_connection(con)
  # remove JSON
  parsedItems <- oefenwebItemJsonParser::itemJsonParser(itemsWithTagsLG,
                                                        withFeedback = TRUE)
  return(parsedItems)
}
