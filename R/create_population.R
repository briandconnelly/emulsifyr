#' Create a population
#'
#' @param size The total carrying capacity for the population
#' @param ndroplets The number of droplets created when emulsifying
#' @param pyield The initial proportion of Yield types
#'
#' @return \code{create_population} returns a tibble representing a population
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- create_population(size = 1e6, ndroplets = 1e4, prop_yield = 0.5)}
#'
create_population <- function(size, ndroplets, prop_yield = 0.5) {
    population = tibble(Rate = rep.int(0, ndroplets),
                        Yield = rep.int(0, ndroplets))
    population[1,] = rbinom(n = 2, size = size, prob = c(1 - pyield, pyield))
    population
}

#' @rdname create_population
#' @param population An emulsifyr population
#' @return \code{popsize} returns the current number of organisms in the given
#' population
#' @export
popsize <- function(population) sum(population)


#' @rdname create_population
#' @return \code{num_droplets} returns the number of droplets into which the
#' population can be emulsified
#' @export
num_droplets <- function(population) nrow(population)


#' @rdname create_population
#' @return \code{is_empty} returns whether the population contains no
#' individuals (\code{TRUE}) or not (\code{FALSE}).
#' @export
is_empty <- function(population) sum(population) == 0
