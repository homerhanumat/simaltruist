#' Simulate Spread of Gene for Altruism
#'
#' @param average_litter_size Mean size of a litter.
#' @param initial_males Number of males in initial population.
#' @param initial_alt_males Number of males in initial population with
#' two alleles for altruism (all other initial males have 0 such alleles).
#' @param initial_females Number of females in initial population.
#' @param initial_alt_males Number of females in initial population with
#' two alleles for altruism (all other initial females have 0 such alleles).
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
#'
#' @return A data frame with information on the population at
#' each generation.
#' @export
simulate<- function(average_litter_size = 5,
                   initial_males = 100,
                   initial_alt_males = 10,
                   initial_females = 100,
                   initial_alt_females = 10,
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
    initial_males = initial_males,
    initial_alt_males = initial_alt_males,
    initial_females = initial_females,
    initial_alt_females = initial_alt_females
    )
  maxId <- max(as.numeric(individuals$id))
  population <- popInit(individuals, sim_gens)
  relMatrix <- relMatrixInit(individuals)

  # go through the generations
  for (i in 1:sim_gens) {

    ## reproduce:
    if (population$males[i] > 0 & population$females[i] > 0) {
      # compute number of couples
      targetChildren <- birth_rate_natural * nrow(individuals)
      number_of_couples <- ceiling(targetChildren / average_litter_size)
      # make the new generation:
      lst <- reproduce(average_litter_size = average_litter_size,
                       number_of_couples = number_of_couples,
                       individuals = individuals,
                       relMatrix = relMatrix,
                       mating_behavior = mating_behavior,
                       maxId = maxId)
      individuals <- lst$individuals
      relMatrix <- lst$relMatrix
      popAdjustment <- lst$popAdjustment
      maxId <- lst$maxId
      population[i + 1, ] <- colSums(rbind(population[i, ], popAdjustment))
    } else {
      population[i + 1, ] <- population[i, ]
    }

    ## cull
    #compute death rate
    dr <- getDeathRate(popSize = population[i + 1, 1],
                    capacity = capacity,
                    death_rate_natural = death_rate_natural,
                    birth_rate_natural = birth_rate_natural)
    lst <- cull(dr = dr,
                individuals = individuals,
                relMatrix = relMatrix)
    individuals <- lst$individuals
    relMatrix <- lst$relMatrix
    popAdjustment <- lst$popAdjustment
    population[i + 1, ] <- colSums(rbind(population[i + 1, ], popAdjustment))


    ## will there be an attack?
    attackOccurs <- runif(1) < prob_attack
    # handle attack, if needed:
    if (attackOccurs) {
      lst <- attack(
        individuals = individuals,
        warner_death_prob = warner_death_prob,
        nonwarner_death_prob = nonwarner_death_prob,
        hider_death_prob = hider_death_prob,
        attack_behavior = attack_behavior,
        relMatrix = relMatrix)
      individuals <- lst$individuals
      relMatrix <- lst$relMatrix
      popAdjustment <- lst$popAdjustment
      population[i + 1, ] <- colSums(rbind(population[i + 1, ], popAdjustment))
    }
  }
  if (graph) {
    altProp <- with(
      population,
      (2*(males2 + females2) + males1 + females1)/(males + females))
    generation <- 0:sim_gens
    df <- data.frame(generation, altProp)
    p <-
      ggplot(df, aes(x = generation, y = altProp)) +
      geom_line()
    print(p)
  }
  return(population)
}
