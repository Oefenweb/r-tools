% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oefenwebTools.R
\docType{package}
\name{oefenwebTools}
\alias{oefenwebTools}
\alias{oefenwebTools-package}
\title{oefenwebTools: A package that contains functionality related to Oefenweb}
\description{
The oefenwebTools package provides functionality related to Oefenweb tools. It
contains for example a function called "getItems" which can be used to retrieve
items from item database with attached tags.
}
\section{getItems function}{
 The oefenwebTools package contains the function
getItems which can be used to obtain items from the Oefenweb item database,
attached tags to the items if available and to remove any JSON format in order to
make item questions and answers readable. The only input variables for the getItems
function are the domain id and status of items (optionally with default status = 1 (active))
}

\section{convertRating function}{
 Function to insert either a "normal" theta user domain
rating, a transformed rating or a Q-score. The input rating is converted into all three
different ratings and printed to the console.
}

\section{Progressor function}{
 Function to track progress of a for loop.
}

\section{lintrProfile function}{
 Function to lint R scripts with Oefenweb default lintr options.
}

\section{createDir function}{
 Function to create directory on hard disk if it does not exist.
}

\section{UpperCase function}{
 Capitalizes all first letters of a word in a strings (or verctor of strings).
}

\section{ExtractDigits function}{
 Extracts digits from string.
}

