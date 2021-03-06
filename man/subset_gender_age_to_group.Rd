% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_checks.R
\name{subset_gender_age_to_group}
\alias{subset_gender_age_to_group}
\title{Function to check the gender column and age column subset based on 
the values in it
have used) in a data frame}
\usage{
subset_gender_age_to_group(data, gender, agelimit)
}
\arguments{
\item{data}{a data frame}

\item{gender}{groupby gender either male or female expected}

\item{agelimit}{list of ages e.g. c(10,20)}
}
\value{
the column number
}
\description{
Function to check the gender column and age column subset based on 
the values in it
have used) in a data frame
}
\examples{
subset_gender_age_to_group(data.frame(age = rep(20, 4), gender = 
rep("male", 4)), "male", c(10, 70))
}
