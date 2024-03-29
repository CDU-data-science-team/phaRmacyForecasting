% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_quantity.R
\name{drug_quantity}
\alias{drug_quantity}
\title{Return the amount of a drug to be ordered}
\usage{
drug_quantity(
  forecast,
  distribution,
  min_stock,
  max_stock,
  p_min,
  p_max,
  inv_i,
  delta_pref,
  outstanding_orders
)
}
\arguments{
\item{forecast}{dataframe with two columns- mean_demand and sd_demand}

\item{distribution}{distr6 object defining the distribution of order arrival}

\item{min_stock}{number level below which we want a probability < p_min of falling}

\item{max_stock}{number level above which we want a prob < p_max of rising}

\item{p_min}{number probability of going into emergency stock}

\item{p_max}{number probability of ordering too much}

\item{inv_i}{int the inventory at the time of this (the i'th) order}

\item{delta_pref}{int the preferred order interval for drug in question}

\item{outstanding_orders}{int the number of units in outstanding orders}
}
\value{
number giving amount to order
}
\description{
Return the amount of a drug to be ordered
}
