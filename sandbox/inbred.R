library(ggplot2)
library(simaltruist)
library(purrr)

randomMatches2 <- function(individuals, relMatrix, current_gen, number_of_couples) {
  select <- upper.tri(relMatrix, diag = TRUE)
  if (current_gen %% 10 == 1) {
    relList[[(current_gen - 1)/10]] <<- relMatrix[select]
  }
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

gens <- 4000
relList <- list()
pop <- simulate(sim_gens = gens,
                capacity = 500,
                prob_attack = 0,
                mating_behavior = list(
                  fn = randomMatches2,
                  args = list()
                ))

means <- map_dbl(relList, mean)
df <- data.frame(gen = 1:floor(gens/10), meanRel = means)
ggplot(df, aes(x = gen, y = meanRel)) +
  geom_line() +
  labs(x = "generation/10")

rels <- data.frame(degree = relList[[floor(gens/10)]])
ggplot(rels, aes(x = degree)) +
  geom_density(fill = "burlywood")
