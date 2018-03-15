## miscellaneous
if(getRversion() >= "2.15.1") utils::globalVariables(c("sex", "warner"))

individualInit <- function(initial_males = 100,
                           initial_alt_males = 10,
                           initial_females = 100,
                           initial_alt_females = 10) {

  initSize <- initial_males + initial_females
  id <- as.character(1:initSize)
  sex <- c(rep("F", times = initial_females),
           rep("M", times = initial_males))
  warner <- c(rep(2, times = initial_alt_females),
              rep(0, times = initial_females - initial_alt_females),
              rep(2, times = initial_alt_males),
              rep(0, times = initial_males - initial_alt_males))
  mom <- rep(NA_character_, times = initial_females)
  dad <- rep(NA_character_, times = initial_males)
  data.frame(id, sex, warner, mom, dad, stringsAsFactors = FALSE)

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
