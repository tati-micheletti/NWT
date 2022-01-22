# OLD CODE TRIALS -- 8_hotspotsPOstdocMS1

# PREVIOUS TRIAL
############################ 
# Previous trial

boo_birdGroups2 <- dcast(boo_birdGroups, 
                         Species + Year + ProportionAreaChosen ~ PrioritizedFor, 
                         value.var = c("ProportionIndividualsConservedMean", 
                                       "ProportionIndividualsConservedMin", 
                                       "ProportionIndividualsConservedMax"))

boo_birdGroups2[, umbrellaIndex := ProportionIndividualsConservedMean_caribou - ProportionIndividualsConservedMean_random]
boo_birdGroups2[, umbrellaIndexMin := ProportionIndividualsConservedMin_caribou - ProportionIndividualsConservedMin_random]
boo_birdGroups2[, umbrellaIndexMax := ProportionIndividualsConservedMax_caribou - ProportionIndividualsConservedMax_random]

for (sp in unique(boo_birdGroups2[["Species"]])){
  boo_birdGroups2[Species == sp, comparisonIndex := get(paste0("ProportionIndividualsConservedMean_", 
                                                               sp)) - ProportionIndividualsConservedMean_random]
  boo_birdGroups2[Species == sp, comparisonIndexMin := get(paste0("ProportionIndividualsConservedMin_", 
                                                                  sp)) - ProportionIndividualsConservedMin_random]
  
  boo_birdGroups2[Species == sp, comparisonIndexMax := get(paste0("ProportionIndividualsConservedMax_", 
                                                                  sp)) - ProportionIndividualsConservedMax_random]
  
}
boo_birdGroups2 <- boo_birdGroups2[, c("Species", "Year", "ProportionAreaChosen", 
                                       "umbrellaIndex", "comparisonIndex",
                                       "umbrellaIndexMin", "umbrellaIndexMax",
                                       "comparisonIndexMin", 
                                       "comparisonIndexMax")]

boo_birdGroupsG1 <- melt(data = boo_birdGroups2, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"),
                         measure.vars = c("umbrellaIndex", "comparisonIndex"))
names(boo_birdGroupsG1)[names(boo_birdGroupsG1) == "variable"] <- "Index"

boo_birdGroupsG2 <- melt(data = boo_birdGroups2, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"),
                         measure.vars = c("umbrellaIndexMin", "comparisonIndexMin"))
names(boo_birdGroupsG2)[names(boo_birdGroupsG2) == "variable"] <- "Index"
names(boo_birdGroupsG2)[names(boo_birdGroupsG2) == "value"] <- "valueMin"
boo_birdGroupsG2[Index == "umbrellaIndexMin", Index := "umbrellaIndex"]
boo_birdGroupsG2[Index == "comparisonIndexMin", Index := "comparisonIndex"]

boo_birdGroupsG3 <- melt(data = boo_birdGroups2, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"),
                         measure.vars = c("umbrellaIndexMax", "comparisonIndexMax"))
names(boo_birdGroupsG3)[names(boo_birdGroupsG3) == "variable"] <- "Index"
names(boo_birdGroupsG3)[names(boo_birdGroupsG3) == "value"] <- "valueMax"
boo_birdGroupsG3[Index == "umbrellaIndexMax", Index := "umbrellaIndex"]
boo_birdGroupsG3[Index == "comparisonIndexMax", Index := "comparisonIndex"]

boo_birds <- merge(boo_birdGroupsG1, boo_birdGroupsG2, 
                   by = c("Species", "Year", "ProportionAreaChosen", "Index"))
boo_birds <- merge(boo_birds, boo_birdGroupsG3, 
                   by = c("Species", "Year", "ProportionAreaChosen", "Index"))

index_plot <- ggplot(data = boo_birds, aes(x = ProportionAreaChosen,
                                           color = Index)) +
  geom_line(aes(y = value), size = 1.3) +
  facet_grid(cols = vars(Year), rows = vars(Species), scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = valueMin,
                  ymax = valueMax,
                  color = Index
  ), alpha = 0.1, colour = NA) +
  scale_x_continuous(breaks = seq(0.05, 0.95, by = 0.1),
                     limits = c(0, 1), expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-0.2, 0.4, by = 0.1),
  #                    limits = c(-0.2, 0.4), expand = c(0,0)) +
  xlab("Proportion of total area protected") + 
  ylab("Difference in proportion of forecasted habitat suitability conserved to randomly protected areas") +
  scale_colour_discrete(name  = "Index",
                        breaks = c("umbrellaIndex", "comparisonIndex"),
                        labels = c("Umbrella", "Reference")) +
  theme(legend.position = "bottom")
index_plot



### TO DOUBLE CHECK BIRDS -- here we can see how adjusted each species is to its grouping

DTind2 <- dcast(DTind,
                Species + Year + ProportionAreaChosen + Habitat ~ PrioritizedFor, 
                value.var = c("ProportionIndividualsConservedMean", 
                              "ProportionIndividualsConservedMin", 
                              "ProportionIndividualsConservedMax"))


DTind2[, umbrellaIndex := ProportionIndividualsConservedMean_caribou - ProportionIndividualsConservedMean_random]
for (sp in unique(DTind2[["Habitat"]])){
  
  DTind2[Habitat == sp, comparisonIndex := get(paste0("ProportionIndividualsConservedMean_", 
                                                      sp)) - ProportionIndividualsConservedMean_random]
}

DTind2 <- DTind2[, c("Species", "Year", "ProportionAreaChosen", "Habitat",
                     "umbrellaIndex", "comparisonIndex")]

DTind2OK <- melt(data = DTind2, 
                 id.vars = c("Species", "Year", "ProportionAreaChosen", "Habitat"),
                 measure.vars = c("umbrellaIndex", "comparisonIndex"))

### HERE I NEED TO PLOT EACH GROUP INDIVIDUALLY

for (group in unique(DTind2OK[["Habitat"]])){
  
  indIndex_plot <- ggplot(data = DTind2OK[Habitat == group, ], aes(x = ProportionAreaChosen, 
                                                                   group = variable,
                                                                   color = variable)) +
    geom_line(aes(y = value), size = 1.3) + 
    facet_grid(cols = vars(Year), rows = vars(Species), scales = "free_y") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0.05, 0.95, by = 0.1),
                       limits = c(0, 1), expand = c(0,0)) +
    # scale_y_continuous(breaks = seq(-0.2, 0.4, by = 0.1),
    #                    limits = c(-0.2, 0.4), expand = c(0,0)) +
    xlab("Proportion of total area protected") + 
    ylab("Difference in proportion of forecasted habitat suitability conserved to randomly protected areas") +
    scale_colour_discrete(name  = "Index",
                          breaks = c("umbrellaIndex", "comparisonIndex"),
                          labels = c("Umbrella", "Reference")) +
    theme(legend.position = "bottom")
  indIndex_plot
}
