## Modules Plot
library("data.table")
library("ggplot2")
library("reproducible")

urlTable <- "https://drive.google.com/open?id=1D2Ivr7vJ_u8C_RambOiG8elRb29IjCE9"

maxVersion <- 2
dt <- reproducible::preProcess(url = urlTable, targetFile = "moduleStatusTable.csv", 
                               destinationPath = file.path(getwd(), "figures/"))
dt <- data.table::fread(dt$targetFilePath)
# dt <- reproducible::prepInputs(url = urlTable, targetFile = "moduleStatusTable.csv", 
#                        destinationPath = file.path(getwd(), "figures/"), fun = "data.table::fread")

names(dt)[names(dt) == "colors"] <- "plotColors"
composedMilestone <- paste0(rep(0:maxVersion, each = 6), c("D","P","T","R","V","M"))
dt[, composedMilestone  := factor(paste0(version, milestone), composedMilestone)]
dt[, groupOrFamily  := factor(groupOrFamily, unique(dt$groupOrFamily))]
dt[, specificComponent  := factor(specificComponent, unique(dt$specificComponent))]

data.table::setkey(dt, groupOrFamily)

p <- ggplot(data = dt, mapping = aes(x = specificComponent, y = composedMilestone)) +
  geom_col(aes(fill = groupOrFamily), position = position_dodge2(reverse = TRUE)) +
  coord_flip() +
  # scale_fill_manual(values = dt$plotColors) + 
  scale_fill_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE)

# how is below diff from above?
p <- ggplot(data = dt, mapping = aes(x = specificComponent, y = composedMilestone)) +
  geom_col(aes(fill = groupOrFamily), position = position_dodge2(reverse = TRUE)) +
  coord_flip() +
  scale_fill_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE)
p
