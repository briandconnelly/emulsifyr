% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_population.R
\name{create_population}
\alias{create_population}
\alias{is_empty}
\alias{num_droplets}
\alias{popsize}
\title{Create a population}
\usage{
create_population(size, ndroplets, prop_yield = 0.5)

popsize(population)

num_droplets(population)

is_empty(population)
}
\arguments{
\item{size}{The total carrying capacity for the population}

\item{ndroplets}{The number of droplets created when emulsifying}

\item{population}{An emulsifyr population}

\item{pyield}{The initial proportion of Yield types}
}
\value{
\code{create_population} returns a tibble representing a population

\code{popsize} returns the current number of organisms in the given
population

\code{num_droplets} returns the number of droplets into which the
population can be emulsified

\code{is_empty} returns whether the population contains no
individuals (\code{TRUE}) or not (\code{FALSE}).
}
\description{
Create a population
}
\examples{
\dontrun{
p1 <- create_population(size = 1e6, ndroplets = 1e4, prop_yield = 0.5)}

}

