## miscellaneous
if(getRversion() >= "2.15.1") utils::globalVariables(c("sex", "warner"))

individualInit <- function(initial_pop) {
  m0 <- initial_pop$m0
  m1 <- initial_pop$m1
  m2 <- initial_pop$m2
  f0 <- initial_pop$f0
  f1 <- initial_pop$f1
  f2 <- initial_pop$f2
  initSize <- m0 + m1 + m2 + f0 + f1 + f2
  id <- as.character(1:initSize)
  sex <- c(rep("F", times = f0 + f1 + f2),
           rep("M", times = m0 + m1 + m2))
  warner <- c(rep(0, times = f0),
              rep(1, times = f1),
              rep(2, times = f2),
              rep(0, times = m0),
              rep(1, times = m1),
              rep(2, times = m2))
  mom <- rep(NA_character_, times = f0 + f1 + f2)
  dad <- rep(NA_character_, times = m0 + m1 + m2)
  df <- data.frame(id, sex, warner, mom, dad, stringsAsFactors = FALSE)
  df
}

popInit <- function(individuals, generations) {
  populationSize <- nrow(individuals)
  males <- sum(individuals$sex == "M")
  females <- populationSize - males
  males0 <- with(individuals, sum(sex == "M" & warner == 0))
  males1 <- with(individuals, sum(sex == "M" & warner == 1))
  males2 <- males - males0 - males1
  females0 <- with(individuals, sum(sex == "F" & warner == 0))
  females1 <- with(individuals, sum(sex == "F" & warner == 1))
  females2 <- females - females0 - females1
  initialPop <- data.frame(populationSize,
                           males, males0, males1, males2,
                           females, females0, females1, females2)
  initialColumn <- rep(NA_integer_, times = generations + 1)
  population <- data.frame(
    populationSize = initialColumn,
    males = initialColumn,
    males0 = initialColumn,
    males1 = initialColumn,
    males2 = initialColumn,
    females = initialColumn,
    females0 = initialColumn,
    females1 = initialColumn,
    females2 = initialColumn
  )
  population[1, ] <- initialPop
  population
}

relMatrixInit <- function(individuals) {
  n <- nrow(individuals)
  relMatrix <- diag(1, nrow = n, ncol = n)
  rownames(relMatrix) <- individuals$id
  colnames(relMatrix) <- individuals$id
  relMatrix
}

getDeathRate <- function(popSize, capacity,
                      death_rate_natural, birth_rate_natural) {
  (birth_rate_natural - death_rate_natural)/capacity * popSize + death_rate_natural
}

relGraphInit <- function(individuals) {
  make_empty_graph() + vertices(individuals$id)
}

# function to adjust population after births or deaths
popAdjust <- function(sex, warner) {

  # really number of new individuals:
  populationSize <- length(sex)

  males <- sum(sex == "M")
  males0 <- sum(sex == "M" & warner == 0)
  males1 <- sum(sex == "M" & warner == 1)
  males2 <- males - males0 - males1
  females <- sum(sex == "F")
  females0 <- sum(sex == "F" & warner == 0)
  females1 <- sum(sex == "F" & warner == 1)
  females2 <- females - females0 - females1
  data.frame(populationSize,
             males, males0, males1, males2,
             females, females0, females1, females2)
}

# compute list of provided arguments for
# mating, attack and cull custom functions
makeProvidedArgs <- function(pvd, fn) {
  vars <- names(formals(fn))
  provideable <- names(pvd)
  neededVars <- intersect(vars, provideable)
  providedArgs <- list()
  for (var in neededVars) {
    providedArgs[[var]] <- get(var, pos = pvd)
  }
  providedArgs
}
