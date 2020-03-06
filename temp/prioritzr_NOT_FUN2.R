library(prioritizr)

# Module: will have each one of the points below as an event.
# Prep inputs all data in the init. Convert all to data frame
# Have parameters for targets

# A few parameters on 
# 1. Input layers needed:

# 1.1. Planning unit data: raster of the area we should plan for (BCR * PROV)
# --> RTM excluding water and excluding anthropogenic layer

# 1.2. Features data: raster stack of the below
# -->  predictions of caribouRSF
# -->  predictions of CAWA
# -->  predictions of OSFL
# -->  predictions of RUBL
# -->  predictions of diversity
# -->  predictions of richness

p <- problem(planningUnit, featuresStack)

# 2. Objectives:
p <- add_min_set_objective(p) 
# Minimum set objective: Minimize the cost of the solution whilst ensuring 
# that all targets are met (Rodrigues et al. 2000). This objective is similar to that used in Marxan 
# (Ball et al. 2009). For example, we can add a minimum set objective to a problem using the 
# following code.

# 3. Rules/Targets:
# create a problem with targets which specify that we need 10 % of the habitat
# for the first feature, 15 % for the second feature, 20 % for the third feature
# 25 % for the fourth feature and 30 % of the habitat for the fifth feature
targets <- c(0.35, 0.25, 0.25, 0.25, 0.3, 0.3)
p <- add_relative_targets(p, targets)


# 4. Constraints:
p <- add_locked_in_constraints(p, edehzhie)#: raster with existing conservation units
# add_neighbor_constraints: guarantee a minimum size for each protected area
# add_contiguity_constraints: make sure all units are contiguous
# add_feature_contiguity_constraints: make sure all units are contiguous for each feature!
# add_locked_out_constraints(raster): can be used to lock areas out of the solution. Potentially for 
# scenario planning -- i.e. mining?

# 5. Penalties
# add_boundary_penalties(penalty = 0.01):  penalize solutions that are excessively fragmented. 
# These penalties are similar to those used in Marxan (Ball et al. 2009; Beyer et al. 2016).

# create problem with penalties that favor combinations of planning units with
# high connectivity, here we will use only the first four layers in
# sim_features for the features and we will use the fifth layer in sim_features
# to represent the connectivity data, where the connectivity_matrix function
# will create a matrix showing the average strength of connectivity between
# adjacent planning units using the data in the fifth layer of sim_features
p <- add_boundary_penalties(p, penalty = 5,
                       data = connectivity_matrix(planningUnit,
                                                  tekMap))
# This can be used for TEK informed maps! i.e. where do caribou cross?

# 6. Decisions
p <- add_binary_decisions(p)#: each pixel can only be conserved or not

# 7. Portifolio
p <- add_pool_portfolio(p, method = 2, number_solutions = 10)

# Solver: 
p <- add_gurobi_solver(p, gap = 0)

conservationUnits <- solve(p) # Outputs a raster if original is a raster.

# Should be a table, though