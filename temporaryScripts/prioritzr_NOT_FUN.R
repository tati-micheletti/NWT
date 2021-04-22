# https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html

if (!require(devtools))
  install.packages("devtools")
devtools::install_github("prioritizr/prioritizr")
# load package
library(prioritizr)


######################################################## USING RASTER

# load feature data
data(sim_features)


# load raster planning unit data
data(sim_pu_raster)

# plot the distribution of suitable habitat for each feature
plot(sim_features, main = paste("Feature", seq_len(nlayers(sim_features))),
     nr = 2, box = FALSE, axes = FALSE)

# create problem
p1 <- problem(sim_pu_raster, sim_features)

# print problem
print(p1)

number_of_planning_units(p1)

######################################################## USING SHAPEFILES

# load planning unit data
data(sim_pu_polygons)

# show the first 6 rows in the attribute table
head(sim_pu_polygons@data)

# plot the planning units and color them according to acquisition cost
spplot(sim_pu_polygons, "cost", main = "Planning unit cost",
       xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))

# create problem with spatial vector data
# note that we have to specify which column in the attribute table contains
# the cost data
p2 <- problem(sim_pu_polygons, sim_features, cost_column = "cost")

# print problem
print(p2)


######################################################## USING DATA FRAME

# We can also initialize a conservation planning problem using tabular planning unit 
# data (i.e. a data.frame). Since the tabular planning unit data does not contain any
#  spatial information, we also have to provide the feature data in tabular format 
#  (i.e. a data.frame) and data showing the amount of each feature in each planning 
#  unit in tabular format (i.e. a data.frame). The feature data must have an "id" 
#  column containing a unique integer identifier for each feature, and the planning 
#  unit by feature data must contain the following three columns: "pu" corresponding 
#  to the planning unit identifiers, "species" corresponding to the feature identifiers, 
#  and "amount" showing the amount of a given feature in a given planning unit.
# set file path for feature data
spec_path <- system.file("extdata/input/spec.dat", package = "prioritizr")

# load in feature data
spec_dat <- data.table::fread(spec_path, data.table = FALSE)

# print first six rows of the data
# note that it contains extra columns
head(spec_dat)

# set file path for planning unit vs. feature data
puvspr_path <- system.file("extdata/input/puvspr.dat", package = "prioritizr")

# load in planning unit vs feature data
puvspr_dat <- data.table::fread(puvspr_path, data.table = FALSE)

# print first six rows of the data
head(puvspr_dat)

# create problem
p3 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvspr_dat)

# print problem
print(p3)

########################################################
########################################################
########################################################

# OBJECTIVE

# create a new problem that has the minimum set objective
p3 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective()

# print the problem
print(p3)

# create a new problem that has the maximum coverage objective and a budget
# of 5000
p4 <- problem(sim_pu_raster, sim_features) %>%
  add_max_cover_objective(5000)

# print the problem
print(p4)

# Maximum features objective: Fulfill as many targets as possible while ensuring that 
# the cost of the solution does not exceed a budget (inspired by Cabeza & Moilanen 2001). 
# This object is similar to the maximum cover objective except that we have the option of
# later specifying targets for each feature. In practice, this objective is more useful 
# than the maximum cover objective because features often require a certain amount of area 
# for them to persist and simply capturing a single instance of habitat for each feature 
# is generally unlikely to enhance their long-term persistence.

# create a new problem that has the maximum features objective and a budget
# of 5000
p5 <- problem(sim_pu_raster, sim_features) %>%
  add_max_features_objective()

# print the problem
print(p5)

# Add targets
# Relative targets: Targets are set as a proportion (between 0 and 1) of the total amount of 
# each feature in the study area. For example, if we had binary feature data and the feature 
# occupied a total of 20 planning units in the study area, we could set a relative target of 
# 50 % to specify that the solution must secure 10 planning units for the feature. We could 
# alternatively specify an absolute target of 10 to achieve the same result, but sometimes 
# proportions are easier to work with.

# create a problem with the minimum set objective and relative targets of 10 %
# for each feature, each component is relative to each feature (% conserved for each one)
targets <- c(0.35, 0.15, 0.2, 0.25, 0.3)
p12 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets)

# print problem
print(p11)


# A constraint can be added to a conservation planning problem to ensure that all solutions 
# exhibit a specific property. For example, they can be used to make sure that all solutions 
# select a specific planning unit or that all selected planning units in the solution follow 
# a certain configuration.

# The following constraints can be added to conservation planning problems in the prioritizr R package.
# 
# Locked in constraints: Add constraints to ensure that certain planning units are prioritized in the 
# solution. For example, it may be desirable to lock in planning units that are inside existing 
# protected areas so that the solution fills in the gaps in the existing reserve network.

# To lock in conservation units, for example:
data(sim_locked_in_raster)

# To lock out units, for example anthropogenic:
data(sim_locked_out_raster)

# create problem with constraints which specify that the first planning unit
# must be selected in the solution
p14 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets) %>%
  add_locked_in_constraints(sim_locked_in_raster)

# print problem
print(p14)

# OR, if polygon, the polygon can contain locked_in and locked_out:
# preview first six rows of the attribute table for sim_pu_polygons
head(sim_pu_polygons@data)

# Boundary penalties: Add penalties to penalize solutions that are excessively fragmented. 
# These penalties are similar to those used in Marxan (Ball et al. 2009; Beyer et al. 2016).

# create problem with penalties that penalize fragmented solutions with a
# penalty factor of 0.01
p22 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 0.01)

# print problem
print(p22)

# Connectivity penalties: Add penalties to favor solutions that select combinations of planning 
# units with high connectivity between them. These penalties are similar to those used in 
# Marxan with Zones (Watts et al. 2009; Beyer et al. 2016). This function supports both 
# symmetric and asymmetric connectivities among planning units.

# create problem with penalties that favor combinations of planning units with
# high connectivity, here we will use only the first four layers in
# sim_features for the features and we will use the fifth layer in sim_features
# to represent the connectivity data, where the connectivity_matrix function
# will create a matrix showing the average strength of connectivity between
# adjacent planning units using the data in the fifth layer of sim_features
p23 <- problem(sim_pu_raster, sim_features[[1:4]]) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 5,
                         data = connectivity_matrix(sim_pu_raster,
                                                    sim_features[[5]])) # THIS COULD EVEN BE A TEK INFORMED LAYER: WHERE ARE CARIBOU CROSSING/WALKING, AND WE COULD PROTECT THOSE AREAS MORE

# print problem
print(p23)

# create a problem and specify that Gurobi should be used to solve the problem
# and specify an optimality gap of zero to obtain the optimal solution
p27 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0)

# print problem
print(p27)

# create a problem and specify that a portfolio containing 10 solutions
# should be created using using Bender's cuts
p30 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0) %>%
  add_cuts_portfolio(method = 2, number_solutions = 10) # >>> This means it will spit out 10 possible solutions (near optimal)

# print problem
print(p30)


# formulate the problem
p33 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 500, edge_factor = 0.5) %>%
  add_binary_decisions()

# solve the problem (using the default solver)
s33 <- solve(p33)

