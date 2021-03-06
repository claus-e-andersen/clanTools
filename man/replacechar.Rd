% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clanToolsFunctions.R
\name{replacechar}
\alias{replacechar}
\title{Replace a character in a string with a new character}
\usage{
replacechar("AbE","b","B")
}
\arguments{
\item{x}{is the string}

\item{char}{is the character to be replaced}

\item{newchar}{is the new character}
}
\value{
The string with the replaced character
}
\description{
Find the most common element(s) in a single vector
Many elements may occur the same number of times (equal to the max number of times).
For example, if a vector of unique elements are given to the function, then all
elements occur exactly once. To limit the number of elements that is returned, a
value for max.number.of.elements may be given.
This functions also handles NULL vectors and NA-values.
}
\author{
Claus E. Andersen
}
