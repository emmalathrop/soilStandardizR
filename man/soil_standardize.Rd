% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soil_standardize.R
\name{soil_standardize}
\alias{soil_standardize}
\title{Standardize soil cores by depth}
\usage{
soil_standardize(soilDf, coreNumName = NA, depth0Name = NA, depth1Name = NA)
}
\arguments{
\item{soilDf}{a dataframe that contains soil cores that need to be depth standardized}

\item{coreNumName}{unique identifier of each core (automatically assumed to be coreNum unless otherwise specified)}

\item{depth0Name}{top depths of each increment in soil core (automatically assumed to be depth0 unless otherwise specified)}

\item{depth1Name}{bottom depths of each increment in soil core (automatically assumed to be depth1 unless otherwise specified)}
}
\value{
A data frame with standardized depth values
}
\description{
Standardize soil cores by depth
}
\examples{
NA
}
