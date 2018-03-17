library(simaltruist)
pop <- simulate(sim_gens = 20)

pop <- simulate(sim_gens = 20,
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
pop <- simulate(sim_gens = 400,
                initial_pop = list(m0 = 80, m1 = 10, m2 = 10,
                                   f0 = 80, f1 = 10, f2 = 10),
                warner_death_prob = 0.5)
pop <- simulate(sim_gens = 200,
                culling_behavior = list(
                  fn = weakWarners,
                  args = list()
                ),
                mating_behavior = list(
                  fn = sexualSelection,
                  args = list(
                    prefs = matrix(
                      c(1, 5, 10, 1, 1, 1, 10, 5, 1),
                      nrow = 3,
                      ncol = 3))))
pop <- simulate(sim_gens = 200, prob_attack = 0,
                culling_behavior = list(fn = weakWarners,
                                        args = list()))
