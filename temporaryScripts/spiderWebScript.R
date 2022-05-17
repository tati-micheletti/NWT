Require::Require("fmsb")
Require::Require("googledrive")

pathOut <- file.path(getwd(), "figures")
destPath <- "1wmHMgQrwtsUejOMJ2IAegRW3S71SHMkd" # Google drive folder

objectives <- c("Xt of Carbon sequestration" , 
                "X% Old growth retention" , 
                "X% MPB population\ndeclining" , 
                "X% Fire proofing within\n100km communities" , 
                "Caribou recovery\n(lambda > 1.02)",
                "X% migratory birds protected",
                "Timber supply\nobjectives reached",
                "X% Biodiversity targets achieved")
numScenarios <- 3
numObjectives <- length(objectives)
DT <- as.data.frame(matrix(sample(30:80, numScenarios * numObjectives, 
                                   replace = FALSE), ncol = numObjectives))
colnames(DT) <- objectives
rownames(DT) <- paste(letters[seq(numScenarios)] , sep = " ")

# To use the fmsb package, I have to add 2 lines to the dataframe: 
# the max and min of each topic to show on the plot!
DT <- rbind(rep(100, numObjectives) , rep(0, numObjectives), DT)
#==================
# Plot 1: Default radar chart proposed by the library:
#radarchart(data)
#==================
# Plot 2: Same plot with custom features
flName <- file.path(pathOut, "Tradeoff management values.png")
colors_border <- c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in <- c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
png(filename = flName, 
    width = 1300, height = 1000)
radarchart(DT, axistype=2,  
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex = 2, calcex = 2, palcex = 2,
)
# mtext(outer = TRUE, side = 3, text = paste("Example tradeoff analysis using integrated\n",
#                                            "modular tools, in SpaDES"), line = -2,
#       cex = 2)
legend(x=1, y=1.3, legend = rownames(DT[-c(1,2),]), bty = "n", pch=20 ,
       col=colors_in , text.col = "grey", cex = 2, pt.cex = 5, 
       title = "Management scenario")
dev.off()
drive_upload(flName, as_id(destPath))
