% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d.R
\name{value_5L}
\alias{value_5L}
\title{Function to value EQ-5D-5L scores for any country and group by gender and age}
\usage{
value_5L(
  eq5dresponse_data,
  mo,
  sc,
  ua,
  pd,
  ad,
  country = "England",
  groupby = NULL,
  agelimit = NULL
)
}
\arguments{
\item{eq5dresponse_data}{the data containing eq5d responses}

\item{mo}{column name for EQ-5D-5L mobility}

\item{sc}{column name for response for EQ-5D-5L self care}

\item{ua}{column name for response for EQ-5D-5L usual activities}

\item{pd}{column name for response for EQ-5D-5L pain/discomfort}

\item{ad}{column name for response for EQ-5D-5L anxiety/depression}

\item{country}{country of interest, by default is England}

\item{groupby}{male or female -grouping by gender, default NULL}

\item{agelimit}{vector of ages to show upper and lower limits, default NULL}
}
\value{
index value  if success, negative values for failure
}
\description{
Function to value EQ-5D-5L descriptive system to index value.
}
\examples{
data <- data.frame(
  age = c(10, 20), sex = c("M", "F"),
  mo = c(1, 2), sc = c(1, 2), ua = c(3, 4), pd = c(3, 4), ad = c(3, 4)
)
value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", NULL, c(10, 70))
}
