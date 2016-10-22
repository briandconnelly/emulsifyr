#' @title Dilute a population
#' @description \code{dilute} TODO
#'
#' @param population An emulsifyr population
#' @param survival_prob The probability of each individual surviving
#'
#' @return a population
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- create_population(size = 1e6, ndroplets = 1e4, prop_yield = 0.5) %>% dilute(survival_prob = 0.01)
#' }
dilute <- function(population, survival_prob) {
    population[,c('Rate', 'Yield')] <- c(rbinom(n = NUM_DROPLETS,
                                                size = population$Rate,
                                                prob = survival_prob),
                                         rbinom(n = NUM_DROPLETS,
                                                size = population$Yield,
                                                prob = survival_prob))
    population
}

#' @return \code{dilute_to_density} dilutes a population to a given density (approx.)
#' @rdname dilute
#' @export
dilute_to_density <- function(population, d) {
    dilute(population,
           survival_prob = d / (sum(population$Rate) + sum(population$Yield)))
}