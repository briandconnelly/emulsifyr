#' @title Emulsify and demulsify a population
#' @description \code{emulsify} emulsifies a population, \code{demulsify} demulsifies a population, and \code{is_emulsified} indicates whether or not a population is
#' emulsified
#'
#' @param population An emulsifyr population
#' @return \code{emulsify} returns an emulsified population
#' @seealso \code{\link{num_droplets}}
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- create_population(size = 1e6, ndroplets = 1e4, prop_yield = 0.5)
#' p1_emulsified <- emulsify(population = p1)}
#'
emulsify <- function(population) {
    population[,c('Rate', 'Yield')] <- c(c(rmultinom(n = 1, size = population[[1, "Rate"]], prob = rep(1/nrow(population), nrow(population)))),
                                         c(rmultinom(n = 1, size = population[[1, "Yield"]], prob = rep(1/nrow(population), nrow(population)))))
    population
}


#' @return \code{demulsify} TODO
#' @rdname emulsify
#' @export
demulsify <- function(population) {
    population[1, c('Rate','Yield')] <- c(sum(population$Rate),
                                          sum(population$Yield))
    population[2:nrow(population), c('Rate','Yield')] <- 0
    population
}


#' @return \code{is_emulsified} returns a logical value indicating whether
#' the population is emulsified (\code{TRUE}) or not (\code{FALSE}).
#' @rdname emulsify
#' @export
is_emulsified <- function(population) sum(population[2:nrow(population),]) > 0
