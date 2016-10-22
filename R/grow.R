#' Grow a population
#'
#' TODO
#'
#' @param population
#'
#' @param population An emulsifyr population
#' @return a population
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- create_population(size = 1e6, ndroplets = 1e4, prop_yield = 0.5)
#' p1_grown <- grow(population = p1)}
#'
grow <- function(population) {
    warning("grow() is not implemented.")
    for (droplet in seq_len(nrow(population))) {
        population[droplet, c("Rate", "Yield")] <- c(0,0)
    }
    population
}
