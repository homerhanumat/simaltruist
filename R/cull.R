## function to kill off some individuals

cull <- function(pvd, dr, culling_behavior) {
  if (!is.null(culling_behavior)) {
    providedArgs <- makeProvidedArgs(c(pvd, list(dr = dr)),
                                     culling_behavior$fn)
    deathProb <- do.call(what = culling_behavior$fn,
                         args = c(providedArgs, culling_behavior$args))
  } else {
    deathProb <- dr
  }
  n <- nrow(pvd$individuals)
  dies <- runif(n) < dr
  dead <- subset(pvd$individuals, dies)
  survivors <- subset(pvd$individuals, !dies)
  popAdjustment <- -popAdjust(dead$sex, dead$warner)
  relMatrix <- cutForDeaths(pvd$relMatrix, pvd$individuals$id, dies)
  list(individuals = survivors,
      relMatrix = relMatrix,
      popAdjustment = popAdjustment)
}

## cut down relationship matrix to account for culling;
## use also after an attack
## ids = individual$id
## dies = logical vector that is TRUE if individual should be removed
cutForDeaths <- function(relMatrix, ids, dies) {
  survivorIds <- ids[!dies]
  relMatrix[survivorIds, survivorIds]
}

## sample custom function for culling
## warners are a bit more likely to die
weakWarners <- function(individuals, dr) {
  warner <- individuals$warner
  deathProb <- numeric(length(warner))
  deathProb[warner == 0] <- dr
  deathProb[warner == 1] <- dr * 2
  deathProb[warner == 2] <- dr * 5
  deathProb
}
