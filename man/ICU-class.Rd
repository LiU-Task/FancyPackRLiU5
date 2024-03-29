% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ICU.R
\docType{class}
\name{ICU-class}
\alias{ICU-class}
\alias{ICU}
\title{ICU}
\arguments{
\item{land, }{a data source or category}

\item{module, }{a data form which includes different information}

\item{date, }{special date when the entity existed}
}
\value{
a set rawData including info such as id, name, type and coordinates etc
}
\description{
Given \code{land}, data form \code{module} and \code{date}, show the map of an entity under special period
}

\examples{
x<-ICU$new(land="se-7",date="2015-11")
x$visa("stockholm")
}
