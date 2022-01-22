# install.packages("Require")
library("Require")
Require("reproducible")
Require("prioritizr")
Require("gurobi")
Require("magrittr")

planningUnit <- prepInputs(url = "https://drive.google.com/file/d/18LUUYPafD_T8Ymq4pgbEDYQvUn7_A7Ng",
                           targetFile = "planningUnit.tif", 
                           destinationPath = tempdir())
multFac <- 3
planningUnitCoarse <- raster::aggregate(x = planningUnit, fun = median, fact = multFac)
planningUnitCoarse[!is.na(planningUnitCoarse)] <- na.omit(unique(planningUnitCoarse[]))*multFac

featuresID <- prepInputs(url = "https://drive.google.com/file/d/167inxuThjrGWYRFUV0nuxm8YOkYC-7bW",
                         targetFile = "speciesA.tif", 
                         destinationPath = tempdir(), 
                         fun = "raster::stack")

featuresIDCoarse <- postProcess(x = featuresID, rasterToMatch = planningUnitCoarse, 
                                method = "bilinear")

# No penalty
conservationProblem1 <- problem(x = planningUnitCoarse,
                               features = featuresIDCoarse) %>%
  add_max_utility_objective(budget = 6307468) %>% # 18861279 represents 35% of the total area for 250m res /  for 750m
  add_proportion_decisions() %>%
  # add_binary_decisions() %>%
  add_top_portfolio(number_solutions = 1) %>%
  add_gurobi_solver(gap = 0.2,
                    time_limit = 60*60*24,
                    presolve = 2,
                    threads = floor(parallel::detectCores() * 0.9),
                    first_feasible = TRUE,
                    verbose = TRUE)

# Notes on values for penalty:
penaltyTable <- data.table::data.table(
  timeElapsedSeconds = c(251349, 
                         213, 
                         1, 
                         1, 
                         176, 
                         668,
                         947,
                         4979,
                         1997),
  penaltyValue = c(1, 
                   0.0001, 
                   0.0001, 
                   0.001, 
                   0.001, 
                   0.01,
                   0.05,
                   0.03,
                   0.1),
  decisionType = c("proportional", 
                   "proportional", 
                   "binary", 
                   "binary", 
                   "proportional", 
                   "proportional",
                   "proportional",
                   "proportional",
                   "proportional"),
  results = c("one huge clump with the same values", 
              "too spread out and single pixels", 
              "only solution is zeros",
              "only solution is zeros",
              "slightly better clamping!! Right way...",
              "YES! Good clumping, 27 areas",
              "YAY! Excellent clumping, 6 areas",
              "YAY! Excellent clumping, 8 areas",
              "Proabbly YAY and something between 1 and 6"),
  solution = c("reduce penalty, decision type should be binary", 
               "try binary decisions to compare running time", 
               "increase penalty -- but might need to go back to proportional decisions", 
               "go back to proportional decisions",
               "increase penalty by 10x",
               "increase penalty by 5x",
               "decrease penalty by half",
               "increase penalty to 0.1",
               "NO MORE?")
)

# On 18OCT21 TM met with EM and SH and we decided that we probably will test penalties for caribou
# between 0.01 and 0.03 (although I would do something between 0.01 and 0.05). This will be
# done for Priority Areas MS #2

conservationProblem1Solved <- prioritizr::solve(conservationProblem1)

conservationProblem2 <- conservationProblem1 %>% add_boundary_penalties(penalty = 0.01,
                                                                        edge_factor = 0.5)
conservationProblem2Solved <- prioritizr::solve(conservationProblem2)

conservationProblem3 <- conservationProblem1 %>% add_boundary_penalties(penalty = 0.05,
                                                                          edge_factor = 0.5)
conservationProblem3Solved <- prioritizr::solve(conservationProblem3)

conservationProblem4 <- conservationProblem1 %>% add_boundary_penalties(penalty = 0.03,
                                                                        edge_factor = 0.5)
conservationProblem4Solved <- prioritizr::solve(conservationProblem4)

conservationProblem5 <- conservationProblem1 %>% add_boundary_penalties(penalty = 0.1,
                                                                        edge_factor = 0.5)
conservationProblem5Solved <- prioritizr::solve(conservationProblem5)

plot(stack(conservationProblem1Solved[[1]], 
           conservationProblem2Solved[[1]],
           conservationProblem3Solved[[1]],
           conservationProblem4Solved[[1]],
           conservationProblem5Solved[[1]]),
           main = c("no penalty", "penalty = 0.01", 
                    "penalty = 0.05", "penalty = 0.03", "penalty = 0.1"))

# Now clumping the solutions to identify the sizes of the clumps
clumpSol1 <- raster::clump(conservationProblem1Solved[["solution_1"]], directions = 8, gaps = TRUE)
clumpSol2 <- raster::clump(conservationProblem2Solved[["solution_1"]], directions = 8, gaps = TRUE)
clumpSol3 <- raster::clump(conservationProblem3Solved[["solution_1"]], directions = 8, gaps = TRUE)
clumpSol4 <- raster::clump(conservationProblem4Solved[["solution_1"]], directions = 8, gaps = TRUE)
clumpSol5 <- raster::clump(conservationProblem5Solved[["solution_1"]], directions = 8, gaps = TRUE)

hist(clumpSol1)
hist(clumpSol2)
hist(clumpSol3)
hist(clumpSol4)
hist(clumpSol5)

PAareas1 <- as.data.table(table(clumpSol1[]))
PAareas2 <- as.data.table(table(clumpSol2[]))
PAareas3 <- as.data.table(table(clumpSol3[]))
PAareas4 <- as.data.table(table(clumpSol4[]))
PAareas5 <- as.data.table(table(clumpSol5[]))

names(PAareas1) <- names(PAareas2) <- names(PAareas3) <- 
  names(PAareas4) <- names(PAareas5) <- 
  c("groupNumber", "priorityAreaSizeInPixels")

# With a 750m resolution, each pixel is 56.25ha big. 
PAareas1[, "areaInHa" := priorityAreaSizeInPixels * 56.25]
PAareas2[, "areaInHa" := priorityAreaSizeInPixels * 56.25]
PAareas3[, "areaInHa" := priorityAreaSizeInPixels * 56.25]
PAareas4[, "areaInHa" := priorityAreaSizeInPixels * 56.25]
PAareas5[, "areaInHa" := priorityAreaSizeInPixels * 56.25]

PAareas1[, "areaInKm2" := priorityAreaSizeInPixels * 0.5625]
PAareas2[, "areaInKm2" := priorityAreaSizeInPixels * 0.5625]
PAareas3[, "areaInKm2" := priorityAreaSizeInPixels * 0.5625]
PAareas4[, "areaInKm2" := priorityAreaSizeInPixels * 0.5625]
PAareas5[, "areaInKm2" := priorityAreaSizeInPixels * 0.5625]

# Patch size had to reach approximately 270 km2 to attain 75% probability of use by caribou. DOI: 10.1002/ece3.695
PAareas1[, "smallerThan270km2" := areaInKm2 < 270]
PAareas2[, "smallerThan270km2" := areaInKm2 < 270]
PAareas3[, "smallerThan270km2" := areaInKm2 < 270]
PAareas4[, "smallerThan270km2" := areaInKm2 < 270]
PAareas5[, "smallerThan270km2" := areaInKm2 < 270]

PAareas1[, "smallerThan10000km2" := areaInKm2 < 10000]
PAareas2[, "smallerThan10000km2" := areaInKm2 < 10000]
PAareas3[, "smallerThan10000km2" := areaInKm2 < 10000]
PAareas4[, "smallerThan10000km2" := areaInKm2 < 10000]
PAareas5[, "smallerThan10000km2" := areaInKm2 < 10000]



