#' Warning Relatives When Under Attack
#'
#' Default function to govern behavior of warners under attack.
#' Warner warns a specified number of the most closely-related
#' individuals.
#' @param individuals Data frame containing id, sex, warner-status,
#' mother id and father id.
#' @param number_warned Maximum number of individuals to warn.
#' @param warner_death_prob As in \code{simulate}.
#' @param nonwarner_death_prob As in \code{simulate}.
#' @param hider_death_prob As in \code{simulate}.
#' @param warnable_relationship  Lowest allowed relationship-degree
#' for an individual to be eligible for warning.
#' @param relMatrix  The current relationship matrix.
#' @param dominant If \code{TRUE}, individuals with 1 or more altruistic
#' alleles can warn.
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
#' Example of an lternative function to govern behavior of warners
#' under attack.
#' Warner warns a specified number of the most closely-related
#' individuals.  Warners only warn other warners.
#' @param individuals Data frame containing id, sex, warner-status,
#' mother id and father id.
#' @param number_warned Maximum number of individuals to warn.
#' @param warner_death_prob As in \code{simulate}.
#' @param nonwarner_death_prob As in \code{simulate}.
#' @param hider_death_prob As in \code{simulate}.
#' @param dominant If \code{TRUE}, individuals with 1 or more altruistic
#' alleles can warn.
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
  individuals,
  warner_death_prob,
  nonwarner_death_prob,
  hider_death_prob,
  attack_behavior,
  relMatrix) {

  if (!is.null(attack_behavior)) {
    fn <- attack_behavior$fn
    vars <- names(formals(fn))
    provideable <- c("individuals",
                     "warner_death_prob",
                     "nonwarner_death_prob",
                     "hider_death_prob",
                     "relMatrix")
    neededVars <- intersect(vars, provideable)
    providedArgs <- list()
    for (var in neededVars) {
      providedArgs[[var]] <- get(var)
    }
    deathProb <- do.call(what = fn,
                         args = c(providedArgs, attack_behavior$args))
  } else {
    deathProb <- warnRelatives(
      individuals,
      number_warned = 10,
      warner_death_prob,
      nonwarner_death_prob,
      hider_death_prob,
      warnable_relationship = 0.25,
      relMatrix,
      dominant = FALSE
    )
  }

  dies <- determineAttackDeath(deathProb)
  dead <- subset(individuals, dies)
  survivors <- subset(individuals, !dies)
  popAdjustment <- -popAdjust(dead$sex, dead$warner)
  relMatrix <- cutForDeaths(relMatrix, individuals$id, dies)
  list(
    individuals = survivors,
    relMatrix = relMatrix,
    popAdjustment = popAdjustment
  )
}
