#### functions pertaining to reproduction

determineSex<- function(n){
  sample(c("M","F"),size= n, replace = T)
}

getChildGenes <- function (mg, dg) {
  parentsum <- mg + dg
  if (parentsum == 0) {
    return(0)
  }
  if (parentsum == 1) {
    return(sample(0:1, size = 1))
  }
  if (parentsum == 2 ) {
    return(sample(c(0,1,2),size=1, prob=c(.25,.5,.25)))
  }
  if (parentsum == 3) {
    return(sample(c(1,2), size=1, prob = c(.5,.5)))
  }
  if (parentsum == 4) {
    return(2)
  }
}

#' Random Mating
#'
#' Default function to govern mating.  Each fertile female mates
#' with a randomly-selected male.  If the user chooses to modify
#' this a as custom function for the \code{mating_behavior}, in
#' \code{simulate} it shold be noted that the current values of
#' of \code{individuals} and \code{number_of_couples} will be
#' provided by the program.
#' @param individuals Data frame containing id, sex, warner-status,
#' mother id and father id.
#' @param number_of_couples Number of couples to form in the current
#' generation.
#' @return A named list with two elements:
#' \describe{
#'   \item{females}{character vector of id numbers of females}
#'   \item{males}{character vector of id numbers of females}
#' }
#' The two vectors are of the same length.  Corresponding elements
#' represent a couple.
#' @export
randomMatches <- function(individuals, number_of_couples) {
  females <- subset(individuals, sex == "F")
  breedingFemales <- females[sample(
    1:nrow(females),
    size = number_of_couples,
    replace = FALSE), ]
  males <- subset(individuals,sex == "M")
  chosenMales <- males[sample(
    1:nrow(males),
    size = nrow(breedingFemales),
    replace = TRUE), ]
  list(females = breedingFemales, males = chosenMales)
}

#' Mating via Sexual Selection
#'
#' Example of alternative function to govern mating.
#' Fertile females of each phenotype have distinct preferences
#' regarding phenotype of a prospective male mate.  The user must
#' set the value of \code{prefs}.
#' @param individuals Data frame containing id, sex, warner-status,
#' mother id and father id.  Value provided.
#' @param number_of_couples Number of couples to form in the current
#' generation.  Value provided.
#' @param prefs Numerical matrix with three rows and three columns. First
#' row gives preferences of female of with 0 altrusits alleles, etc.
#' Thus the row (1, 5, 10) indicates that female is ten five more likely to
#' mate with a given 1-allele male than with a given 0-allele male,
#' and ten times more likely to mate with a 2-allele male than a
#' 0-allele male.
#' @return A list with two named elements:
#' \describe{
#'   \item{females}{character vector of id numbers of females}
#'   \item{males}{character vector of id numbers of females}
#' }
#' The two vectors are of the same length.  Corresponding elements
#' represent a couple.
#' @examples
#' \dontrun{
#' pop <- simulate(sim_gens = 200,
#'                 mating_behavior = list(
#'                   fn = sexualSelection,
#'                   args = list(
#'                     prefs = matrix(
#'                       c(1, 5, 10, 1, 1, 1, 10, 5, 1),
#'                       nrow = 3,
#'                       ncol = 3))))
#' }
#' @export
sexualSelection <- function(individuals,
                            number_of_couples,
                            prefs = matrix(1, nrow = 3, ncol = 3)) {
  breedingFemales <-
    individuals %>%
    filter(sex == "F") %>%
    sample_n(number_of_couples, replace = FALSE) %>%
    arrange(warner)
  warnerf <- factor(breedingFemales$warner, levels = 0:2)
  femaleTallies <- table(warnerf)
  males <- individuals %>%
    filter(sex == "M") %>%
    arrange(warner)
  warnerm <- factor(males$warner, levels = 0:2)
  maleTallies <- table(warnerm)
  prefs0 <- c(
    rep(prefs[1, 1], times = maleTallies["0"]),
    rep(prefs[1, 2], times = maleTallies["1"]),
    rep(prefs[1, 3], times = maleTallies["2"])
  )
  prefs1 <- c(
    rep(prefs[2, 1], times = maleTallies["0"]),
    rep(prefs[2, 2], times = maleTallies["1"]),
    rep(prefs[2, 3], times = maleTallies["2"])
  )
  prefs2 <- c(
    rep(prefs[3, 1], times = maleTallies["0"]),
    rep(prefs[3, 2], times = maleTallies["1"]),
    rep(prefs[3, 3], times = maleTallies["2"])
  )
  chosenMales <- data.frame(
    id = character(number_of_couples),
    sex = character(number_of_couples),
    warner = numeric(number_of_couples),
    mom = character(number_of_couples),
    dad = character(number_of_couples),
    stringsAsFactors = FALSE
  )
  fems0 <- femaleTallies["0"]
  if (fems0 > 0) {
    chosenMales[1:fems0, ] <-
      males %>%
      sample_n(fems0, weight = prefs0, replace = TRUE)
  }
  fems1 <- femaleTallies["1"]
  if (fems1 > 0) {
    chosenMales[(fems0 + 1) : (fems0 + fems1), ] <-
      males %>%
      sample_n(fems1, weight = prefs1, replace = TRUE)
  }
  fems2 <- femaleTallies["2"]
  if (fems2 > 0) {
    chosenMales[(fems0 + fems1 + 1) : number_of_couples, ] <-
      males %>%
      sample_n(fems2, weight = prefs2, replace = TRUE)
  }
  results <- list(females = breedingFemales, males = chosenMales)
  results
}


## function to make a single litter
makeLitter <- function(mom, dad, actualsize, lastId) {

  n <- actualsize
  momId <- mom$id
  dadId <- dad$id
  ids <- (lastId + 1) : (lastId + n)
  sex <- determineSex(n)
  kidMom <- rep(momId, times = n)
  kidDad <- rep(dadId, times = n)
  dadGenes <- dad$warner
  momGenes <- mom$warner

  warner <- numeric(n)
  for (i in 1:n) {
    warner[i]<- getChildGenes(mg = momGenes, dg = dadGenes)
  }

  litter <- data.frame(id = as.character(ids),
                       sex = sex,
                       warner = warner,
                       mom = kidMom,
                       dad = kidDad,
                       stringsAsFactors = FALSE)
  litter
}

reproduce <- function (pvd,
                       number_of_couples,
                       mating_behavior,
                       relationship_method,
                       maxId) {

  ## To compute next row of the populaton data frame:
  popAdjustment <- data.frame(populationSize = 0,
                          males = 0, males0 = 0, males1 = 0, males2 = 0,
                          females = 0, females0 = 0, females1 = 0, females2 = 0)

  ## get the pairs:
  if (!is.null(mating_behavior)) {
    providedArgs <- makeProvidedArgs(c(pvd,
                                       list(
                                         number_of_couples = number_of_couples)
                                       ),
                                     mating_behavior$fn)
    couples <- do.call(what = mating_behavior$fn,
                       args = c(providedArgs, mating_behavior$args))
  } else {
    couples <- randomMatches(pvd$individuals, number_of_couples)
  }

  femaleMates <- couples$females
  maleMates <- couples$males

  momIDs <- femaleMates$id
  dadIDs <- maleMates$id
  litterSizes <- 1 + rpois(number_of_couples, pvd$average_litter_size - 1)
  totalKids <- sum(litterSizes)
  newPopSize <- nrow(pvd$individuals) + sum(litterSizes)

  # prepare a new individuals data frame:
  extIndividuals <- data.frame(
    id = character(newPopSize),
    sex = character(newPopSize),
    warner = numeric(newPopSize),
    mom = character(newPopSize),
    dad = character(newPopSize),
    stringsAsFactors = FALSE
  )
  extIndividuals[1:nrow(pvd$individuals), ] <- pvd$individuals

  if (relationship_method == "matrix") {
    # prepare a new relationship matrix:
    newRelMatSize <- nrow(pvd$individuals) + totalKids
    extRelMat <- matrix(0, nrow = newRelMatSize, ncol = newRelMatSize)
    extRelMat[1:nrow(pvd$individuals), 1:nrow(pvd$individuals)] <- pvd$relMatrix
    rcNames <- c(
      rownames(pvd$relMatrix),
      as.character((maxId + 1):(maxId + totalKids))
    )
    rownames(extRelMat) <- rcNames
    colnames(extRelMat) <- rcNames
  } else if (relationship_method == "graph") {
    # extend the relationship graph all at once to save copying
    allKids <- (maxId + 1):(maxId + totalKids)
    newEdges <- numeric(4 * totalKids)
    counter <- 0
    lastId <- maxId
    for (i in 1:number_of_couples) {
      newEdges[seq(counter + 2, counter + 2 * litterSizes[i], 2)] <-
        seq(lastId + 1, lastId + litterSizes[i], 1)
      newEdges[seq(counter + 1, counter + 2 * litterSizes[i] - 1, 2)] <-
        rep(as.numeric(momIDs[i]), times = litterSizes[i])
      counter <- counter + 2 * litterSizes[i]
      newEdges[seq(counter + 2, counter + 2 * litterSizes[i], 2)] <-
        seq(lastId + 1, lastId + litterSizes[i], 1)
      newEdges[seq(counter + 1, counter + 2 * litterSizes[i] - 1, 2)] <-
        rep(as.numeric(dadIDs[i]), times = litterSizes[i])
      counter <- counter + 2 * litterSizes[i]
      lastId <- lastId + litterSizes[i]
    }
    relGraph <- pvd$relGraph +
      vertices(allKids) +
      edges(newEdges)
  }

  # prepare for loop:
  m <- nrow(pvd$individuals)
  lastId <- maxId

  for (i in 1:number_of_couples) {

    ## get the next litter:
    n <- litterSizes[i]
    litter <- makeLitter(mom = femaleMates[i, ],
                         dad = maleMates[i, ],
                         actualsize = n,
                         lastId = lastId)

    ## enter into new individual data frame:
    extIndividuals[(m + 1):(m + n), ] <- litter

    if (relationship_method == "matrix") {
      ## enter into new relationship matrix
      litterMat <- matrix(0.5, n, n) + diag(0.5, n, n)
      # fill in lower right with litter matrix:
      extRelMat[(m + 1):(m + n), (m + 1):(m + n)] <- litterMat
      # determine relationship of the children with each previous
      # member of the population:
      momId <- femaleMates[i, ]$id
      dadId <- maleMates[i, ]$id
      relation <- 0.5 * (extRelMat[momId, 1:m] + extRelMat[dadId, 1:m])
      # construct matrix to become lower left, and fill in:
      lowerLeft <- matrix(rep(relation, n), nrow = n, byrow = TRUE)
      extRelMat[(m + 1):(m + n), 1:m] <- lowerLeft
      # fill in the upper right:
      extRelMat[1:m, (m + 1):(m + n)] <- t(lowerLeft)
    }

    ## add to population adjustment:
    popAdjustment <- colSums(rbind(
      popAdjustment,
      popAdjust(litter$sex, litter$warner)
      ))

    # prepare for next iteration:
    m <- m + n
    lastId <- lastId + n
  }

  ## return results:
  results <- list(individuals = extIndividuals,
                  maxId = lastId,
                  popAdjustment = popAdjustment)
  if (relationship_method == "matrix") {
    results$relMatrix = extRelMat
  } else if (relationship_method == "graph") {
    results$relGraph <- relGraph
  }
  results
}

