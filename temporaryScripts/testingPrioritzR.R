## Not run:
# set seed for reproducibility
library("Require")
Require("reproducible")
Require("prioritizr")
Require("raster")
Require("gurobi")
Require("magrittr")
# When gurobi is not installed: install.packages('path/to/gurobi/R/R', repos=NULL)
# install.packages("C:/Program Files (x86)/Gurobi/win64/R/gurobi_9.1-2.zip", repos=NULL)
set.seed(500)

# load data
data(sim_pu_raster, sim_features)

# build minimal conservation problem with raster data
prob <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = TRUE)

p2 <- prob %>% add_boundary_penalties(penalty = 3, edge_factor = 0.5) # when suing penalty = 10 
p3 <- prob %>% add_boundary_penalties(penalty = 5, edge_factor = 0.5) # when suing penalty = 10 
p4 <- prob %>% add_boundary_penalties(penalty = 10, edge_factor = 0.5) # when suing penalty = 10 
p5 <- prob %>% add_boundary_penalties(penalty = 1000, edge_factor = 0.5) # when suing penalty = 10
p6 <- prob %>% add_boundary_penalties(penalty = 3, edge_factor = 1) # when suing penalty = 10 

sols <- stack(solve(prob), solve(p2),
              solve(p3), solve(p4),
              solve(p5), solve(p6))

names(sols) <- paste0("p", 1:6)

# calculate boundary associated with the solution
bounds <- lapply(names(sols), function(P){
  r <- eval_boundary_summary(prob, sols[[P]])
  return(r)
})

boundsValue <- unlist(lapply(bounds, `[[`, "boundary"))

plot(sols, main = c(paste0("No penalty; \nboundary = ", boundsValue[1]), 
                    paste0("Penalty = 3; \nboundary = ", boundsValue[2]),
                    paste0("Penalty = 5; \nboundary = ", boundsValue[3]), 
                    paste0("Penalty = 10; \nboundary = ", boundsValue[4]),
                    paste0("Penalty = 1000; \nboundary = ", boundsValue[5]), 
                    paste0("Penalty = 3 high edge-factor; \nboundary = ", boundsValue[6])))

