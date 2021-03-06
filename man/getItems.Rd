% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getItems.R
\name{getItems}
\alias{getItems}
\title{Function to obtain items without JSON format and with tags (if available).}
\usage{
getItems(domainId, itemStatus = 1, dbname = "oefenweb_nl_app")
}
\arguments{
\item{domainId}{An item domain id}

\item{itemStatus}{Item status (in numeric or character),
it is possible to enter several states like this
for example: c(1, 2, 3).}

\item{dbname}{Name of database}
}
\value{
Item dataset with tags (if available) and without JSON formatting
}
\description{
Function to obtain items without JSON format and with tags (if available).
}
