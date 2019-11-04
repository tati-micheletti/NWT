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

## this version shades in the regions of the graph corresponding to "version" and has the correspondent colors to the hex stickers
p <- ggplot() + 
  geom_col(data = dt,
           mapping = aes(x = specificComponent, y = composedMilestone, fill = groupOrFamily),
           position = position_dodge2(reverse = TRUE)) +
  coord_flip() +
  # scale_fill_discrete(drop = FALSE) +
  # scale_y_discrete(value = cols)) +
  geom_hline(yintercept = c(6, 12), lty = 3) +
  ylab(label = "Version and milestone") +
  xlab(label = "Modules") +
  # labs(fill = "Module's families") +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.position = "right",#c(0.88, 0.7),
        legend.background = element_blank()) +
scale_fill_manual("Module's families", values = c("vegetation dynamics - LandR" = "#3A8426", 
                                                  "fire - fireSense" = "#962A2A",
                                                  "fire - SCFM" = "#CC3737", 
                                                  "caribou" = "#E2C026", 
                                                  "birds" = "#637DB5", 
                                                  "biodiversity metrics" = "#3D3D3D", 
                                                  "trends" = "#030077",
                                                  "harvesting" = "#88C95C", 
                                                  "hotspots" = "#5D5BA0", 
                                                  "mountain pine beetle" = "#9B831A", 
                                                  "carbon" = "#939393", 
                                                  "anthropogenic disturbances" = "#07051E"))
p

## this version shades in the regions of the graph corresponding to "version"
rects <- data.frame(ystart = c(0, 6, 12), yend = c(6, 12, 18),
                    col = c("alpha", "beta", "release")) ## TODO: better names
q <- ggplot() + 
  geom_rect(data = rects,
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = ystart, ymax = yend, fill = col),
            alpha = 0.3) +
  geom_col(data = dt,
           mapping = aes(x = specificComponent, y = composedMilestone, fill = groupOrFamily),
           position = position_dodge2(reverse = TRUE)) +
  coord_flip() +
  scale_fill_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) ## TODO: split legend in two: shaded version region and moudle group
q
