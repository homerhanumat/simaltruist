library(simaltruist)
pop <- simulate(sim_gens = 20)

pop <- simulate(sim_gens = 200,
                mating_behavior = list(
                  fn = sexualSelection,
                  args = list(
                    prefs = matrix(
                      c(1, 5, 10, 1, 1, 1, 10, 5, 1),
                      nrow = 3,
                      ncol = 3))))
pop <- simulate(sim_gens = 20,
                attack_behavior = list(
                  fn = warnRelatives,
                  args = list(
                    number_warned = 7,
                    warnable_relationship = 0.1,
                    dominant = TRUE)))
pop <- simulate(sim_gens = 100,
                attack_behavior = list(
                  fn = warnWarners,
                  args = list(
                    number_warned = 7,
                    dominant = TRUE)))
