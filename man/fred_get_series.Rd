% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FREDAPI.R
\name{fred_get_series}
\alias{fred_get_series}
\title{Federal Reserve Economic Data(FRED) API Query}
\usage{
fred_get_series(APIkey, series_id, observation_start)
}
\arguments{
\item{APIkey}{Your FRED API Key}

\item{series_id}{Series ID of the data from FRED}

\item{observation_start}{Start of Observation ("yyyy-mm-dd")}
}
\value{
Data from Specified FRED Query
}
\description{
Federal Reserve Economic Data(FRED) API Query
}
\examples{
fred_get_series("1234567890","UNRATE","1999-01-01")
}
