#' Warning Relatives When Under Attack
#'
#' Default function to govern behavior of warners under attack.
#' Warner warns a specified number of the most closely-related
#' individuals.  May be used as an alternative function in which
#' the values of \code{warnable_relationship} and \code{dominant}
#' are specified.
#' @param individuals Data frame containing id, sex, warner-status,
#' mother id and father id.  Value provided.
#' @param number_warned Maximum number of individuals to warn.
#' Value provided.
#' @param warner_death_prob As in \code{simulate}. Value provided.
#' @param nonwarner_death_prob As in \code{simulate}. Value provided.
#' @param hider_death_prob As in \code{simulate}. Value provided.
#' @param warnable_relationship  Lowest allowed relationship-degree
#' for an individual to be eligible for warning.  Default version
#' sets this to 0.25.
#' @param relMatrix  The current relationship matrix.  Value provided.
#' @param dominant If \code{TRUE}, individuals with 1 or more altruistic
#' alleles can warn.  Default version sets this to \code{FALSE}.
#' @return A numerical vector of death probabilities in which the
#' \eqn{i^{\text{th}}} element corresponds to row \eqn{i} of
#' \code{individuals}.
#' @examples
#' \dontrun{
#' pop <- simulate(sim_gens = 200,
#'                 attack_behavior = list(
#'                   fn = warnRelatives,
#'                   args = list(
#'                     number_warned = 7,
#'                     warnable_relationship = 0.1,
#'                     dominant = TRUE)))
#' }
#' @export
warnRelatives <- function(
                         individuals,
                         number_warned,
                         warner_death_prob,
                         nonwarner_death_prob,
                         hider_death_prob,
                         warnable_relationship,
                         relMatrix,
                         dominant = FALSE) {
  id <- individuals$id
  warner <- individuals$warner
  deathProb <- rep(nonwarner_death_prob, times = length(id))
  warnerCutoff <- ifelse(dominant, 1, 2)
  warners <- id[warner >= warnerCutoff]
  hideable <- id
  # randomly shuffle the warners:
  warners <- sample(warners, replace = FALSE)
  while (length(warners) > 0) {
    warnerId <- warners[1]
    warners <- warners[-1]
    hideable <- hideable[hideable != warnerId]
    warnable <- hideable[relMatrix[warnerId,
                                   hideable]
                         >= warnable_relationship]
    warned <-
      head(warnable[order(relMatrix[warnerId,
                                    warnable],
                          decreasing = TRUE)],
           number_warned)
    if (length(warned) > 0) {
      deathProb[id %in% warned] <- hider_death_prob
      deathProb[id == warnerId] <- warner_death_prob
    } else {
      deathProb[id == warnerId] <- hider_death_prob
    }
    warners <- setdiff(warners, warned)
    hideable <- setdiff(hideable, warned)
  }
  deathProb
}

#' Warning Other Warners
#'
#' Example of an alternative function to govern behavior of warners
#' under attack.  User must set values of \code{number_warned} and
#' \code{dominant}.
#' Warner warns a specified number of the most closely-related
#' individuals.  Warners only warn other warners.
#' @param individuals Data frame containing id, sex, warner-status,
#' mother id and father id.Value provided.
#' @param number_warned Maximum number of individuals to warn.
#' @param warner_death_prob As in \code{simulate}.  Value provided.
#' @param nonwarner_death_prob As in \code{simulate}.  Value provided.
#' @param hider_death_prob As in \code{simulate}.  Value provided.
#' @param dominant If \code{TRUE}, individuals with 1 or more altruistic
#' alleles can warn.
#' @return A numerical vector of death probabilities in which the
#' \eqn{i^{\text{th}}} element corresponds to row \eqn{i} of
#' \code{individuals}.
#' @examples
#' \dontrun{
#' pop <- simulate(sim_gens = 200,
#'                 attack_behavior = list(
#'                   fn = warnWarners,
#'                   args = list(
#'                     number_warned = 7,
#'                     dominant = TRUE)))
#' }
#' @export
warnWarners <- function(
                         individuals,
                         number_warned,
                         warner_death_prob,
                         nonwarner_death_prob,
                         hider_death_prob,
                         dominant) {
  id <- individuals$id
  warner <- individuals$warner
  deathProb <- rep(nonwarner_death_prob, times = length(id))
  warnerCutoff <- ifelse(dominant, 1, 2)
  warners <- id[warner >= warnerCutoff]
  hideable <- id
  # randomly shuffle the warners:
  warners <- sample(warners, replace = FALSE)
  while (length(warners) > 0) {
    warnerId <- warners[1]
    warners <- warners[-1]
    warned <- head(warners, n = 10)
    if (length(warned) > 0) {
      deathProb[id %in% warned] <- 0
      deathProb[id == warnerId] <- warner_death_prob
    } else {
      deathProb[id == warnerId] <- 0
    }
    warners <- setdiff(warners, warned)
    hideable <- setdiff(hideable, warned)
  }
  deathProb
}

## in this version, only warner = 2 warns, but
## all with warner = 1 or 2 can be warned
warners2 <- function(
                         individuals,
                         number_warned,
                         warner_death_prob,
                         nonwarner_death_prob,
                         dominant) {
  id <- individuals$id
  warner <- individuals$warner
  deathProb <- rep(nonwarner_death_prob, times = length(id))
  warnerCutoff <- 2
  warners <- id[warner >= warnerCutoff]
  hideable <- id[warner >= 1]
  # randomly shuffle the warners:
  warners <- sample(warners, replace = FALSE)
  while (length(warners) > 0) {
    warnerId <- warners[1]
    warners <- warners[-1]
    hideable <- hideable[hideable != warnerId]
    warned <- head(hideable, n = 10)
    if (length(warned) > 0) {
      deathProb[id %in% warned] <- 0
      deathProb[id == warnerId] <- warner_death_prob
    } else {
      deathProb[id == warnerId] <- 0
    }
    warners <- setdiff(warners, warned)
    hideable <- setdiff(hideable, warned)
  }
  deathProb
}


## determine who dies
determineAttackDeath <- function(deathProb) {
  randomUnif <- runif(length(deathProb))
  randomUnif < deathProb
}

## attack ---------------

attack <- function(
  pvd,
  attack_behavior) {

  if (!is.null(attack_behavior)) {
    providedArgs <- makeProvidedArgs(pvd, attack_behavior$fn)
    deathProb <- do.call(what = fn,
                         args = c(providedArgs, attack_behavior$args))
  } else {
    deathProb <- warnRelatives(
      pvd$individuals,
      number_warned = 10,
      pvd$warner_death_prob,
      pvd$nonwarner_death_prob,
      pvd$hider_death_prob,
      warnable_relationship = 0.25,
      pvd$relMatrix,
      dominant = FALSE
    )
  }

  dies <- determineAttackDeath(deathProb)
  dead <- subset(pvd$individuals, dies)
  survivors <- subset(pvd$individuals, !dies)
  popAdjustment <- -popAdjust(dead$sex, dead$warner)
  relMatrix <- cutForDeaths(pvd$relMatrix, pvd$individuals$id, dies)
  list(
    individuals = survivors,
    relMatrix = relMatrix,
    popAdjustment = popAdjustment
  )
}
