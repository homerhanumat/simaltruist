#' Simulate Spread of Gene for Altruism
#'
#' @param initial_pop List comprised of six named elements:
#' \describe{
#'   \item{m0}{initial number of males with 0 altruist alleles;}
#'   \item{m1}{initial number of males with 1 altruist allele;}
#'   \item{m2}{initial number of males with 2 altruist alleles;}
#'   \item{f0}{initial number of females with 0 altruist alleles;}
#'   \item{f1}{initial number of females with 1 altruist allele;}
#'   \item{f2}{initial number of females with 2 altruist alleles.}
#' }
#' @param average_litter_size Mean size of a litter.
#' @param birth_rate_natural Birth rate for the population.
#' @param death_rate_natural Death rate for a size-zero population. Rises
#' linearly to \code{birth_rate_natrual} as populaiion approaches
#' \code{capacity}.
#' @param prob_attack The probability of a predator attack in any given
#' generation.
#' @param warner_death_prob Probability that an individual who warns
#' others during an attack will be killed.
#' @param nonwarner_death_prob Probability of death for an individual who
#' does not warn others but who was not forewarned by a warner.
#' @param hider_death_prob Probability of death during an attack for an
#' individual who accepts a warning.
#' @param sim_gens Number of generations to simulate.
#' @param capacity Carrying capacity of the population.
#' @param mating_behavior Custom function to govern how eligible
#' partners form couples.
#' @param attack_behavior Custom function to govern behavior of
#' warners when population is under attack.
#' @param graph If \code{TRUE}, provides a graph of the total
#' per-capita warner alleles in population over the generations.
#' @note For details on \code{mating_behavior} and \code{attack_behavior}
#' consult the
#' \href{https://homerhanumat.github.io/simaltruist}{package documentation}.
#' @return A data frame with information on the population at
#' each generation.  Each row describes the population at the end of
#' a single birth-death-attack cycle.   Variables are:
#' \describe{
#'   \item{populationSize}{total population}
#'   \item{males}{total number of males}
#'   \item{males0}{number of males with no alleles for altruism}
#'   \item{males1}{number of males with one allele for altruism}
#'   \item{males2}{number of males with two alleles for altruism}
#'   \item{females}{total number of females}
#'   \item{females0}{number of females with no alleles for altruism}
#'   \item{females1}{number of females with one allele for altruism}
#'   \item{females2}{number of females with two alleles for altruism}
#' }
#' @examples
#' \dontrun{
#' # use defaults, get a graph:
#' pop <- simulate(sim_gens = 400)
#' # attacks are infrequent, and it's dangerous to warn:
#' pop <- simulate(sim_gens = 200,
#'                 warner_death_prob = 0.8,
#'                 attack_prob = 0.05)
#' # use an alternative mating function exported by package:
#' pop <- simulate(sim_gens = 200,
#'                 warner_death_prob = 0.8,
#'                 mating_behavior = list(
#'                   fn = sexualSelection,
#'                   args = list(
#'                     matrix(
#'                       c(1, 5, 10, 1, 1, 1, 10, 5, 1),
#'                       nrow = 3,
#'                       ncol = 3))))
#' }
#' @export
simulate <- function(
                     initial_pop = list(
                       m0 = 90, m1 = 0, m2 = 10,
                       f0 = 90, f1 = 0, f2 = 10
                     ),
                     average_litter_size = 5,
                     birth_rate_natural = .05,
                     death_rate_natural = .0,
                     prob_attack = .2,
                     warner_death_prob = .4,
                     nonwarner_death_prob = .2,
                     hider_death_prob = 0,
                     sim_gens = 2,
                     capacity = 2000,
                     mating_behavior = NULL,
                     attack_behavior = NULL,
                     graph = TRUE) {

  individuals <- individualInit(
    initial_pop = initial_pop
  )
  maxId <- max(as.numeric(individuals$id))
  population <- popInit(individuals, sim_gens)
  relMatrix <- relMatrixInit(individuals)
  # provideable variables:
  pvd <- list(
    individuals = max(as.numeric(individuals$id)),
    population = popInit(individuals, sim_gens),
    relMatrix = relMatrixInit(individuals),
    average_litter_size = average_litter_size,
    birth_rate_natural = birth_rate_natural,
    death_rate_natural = death_rate_natural,
    prob_attack = prob_attack,
    warner_death_prob = warner_death_prob,
    nonwarner_death_prob = nonwarner_death_prob,
    hider_death_prob = hider_death_prob,
    current_gen = 0,
    capacity = capacity
  )

  # go through the generations
  for (i in 1:sim_gens) {
    pvd$current_gen <- i
    ## reproduce:
    if (pvd$population$males[i] > 0 & pvd$population$females[i] > 0) {
      # compute number of couples
      targetChildren <- birth_rate_natural * nrow(pvd$individuals)
      number_of_couples <- ceiling(targetChildren / average_litter_size)
      # make the new generation:
      lst <- reproduce(
        pvd = pvd,
        number_of_couples = number_of_couples,
        mating_behavior = mating_behavior,
        maxId = maxId
      )
      pvd$individuals <- lst$individuals
      pvd$relMatrix <- lst$relMatrix
      popAdjustment <- lst$popAdjustment
      maxId <- lst$maxId
      pvd$population[i + 1, ] <- colSums(rbind(pvd$population[i, ], popAdjustment))
    } else {
      pvd$population[i + 1, ] <- pvd$population[i, ]
    }

    ## cull
    # compute death rate
    dr <- getDeathRate(
      popSize = pvd$population[i + 1, 1],
      capacity = capacity,
      death_rate_natural = death_rate_natural,
      birth_rate_natural = birth_rate_natural
    )
    lst <- cull(
      dr = dr,
      individuals = pvd$individuals,
      relMatrix = pvd$relMatrix
    )
    pvd$individuals <- lst$individuals
    pvd$relMatrix <- lst$relMatrix
    popAdjustment <- lst$popAdjustment
    pvd$population[i + 1, ] <- colSums(rbind(pvd$population[i + 1, ], popAdjustment))


    ## will there be an attack?
    attackOccurs <- runif(1) < prob_attack
    # handle attack, if needed:
    if (attackOccurs) {
      lst <- attack(
        pvd = pvd,
        attack_behavior = attack_behavior
      )
      pvd$individuals <- lst$individuals
      pvd$relMatrix <- lst$relMatrix
      popAdjustment <- lst$popAdjustment
      pvd$population[i + 1, ] <- colSums(rbind(pvd$population[i + 1, ], popAdjustment))
    }
    if (pvd$population$populationSize[i + 1] == 0) break
  }
  if (graph) {
    altProp <- with(
      pvd$population,
      (2 * (males2 + females2) + males1 + females1) / (males + females)
    )
    generation <- 0:nrow(pvd$population)
    df <- data.frame(generation, altProp)
    p <-
      ggplot(df, aes(x = generation, y = altProp)) +
      geom_line()
    print(p)
  }
  return(pvd$population)
}
