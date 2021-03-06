% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/availableSediment.R
\name{availableSediment}
\alias{availableSediment}
\title{Calculate available sediment}
\usage{
availableSediment(
  floodPct,
  suspendedSediment,
  settlingVelocity,
  capturedSediment = 1
)
}
\arguments{
\item{floodPct}{a numeric, fraction of time per tidal cycle the marsh is inundated}

\item{suspendedSediment}{a numeric, suspended sediment concentration of the water column}

\item{settlingVelocity}{a numeric, the number of times a water column will clear per tidal cycle}

\item{capturedSediment}{numeric, the fraction of available sediment captured by the marsh}
}
\value{
numeric, the available sediment given a set of sediment and flooding conditions
}
\description{
This function calculates the available sediment for a marsh elevation over a tidal cycle.
}
