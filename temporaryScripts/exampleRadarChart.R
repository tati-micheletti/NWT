# Library
Require::Require("fmsb")

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix(sample(2:20 , 5, replace = T), ncol = 5))
colnames(data) <- c("CaribouRSF" , "CaribouLambda" , "Landbirds" , "Waterfowl" , "Carbon")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20, 5) , rep(0, 5) , data)

# Check your data, it has to look like this!
# head(data)

# Custom the radarChart !
radarchart(data, axistype = 1 ,
            #custom polygon
           pcol = rgb(0.2,0.5,0.5,0.9) , 
           pfcol = rgb(0.2,0.5,0.5,0.5) , 
           plwd = 4 ,
            #custom the grid
          cglcol="black", 
          cglty = 2, 
          centerzero = TRUE,
          axislabcol = "black", 
          caxislabels = seq(0,20,5), 
          cglwd = 1,
            #custom labels
          vlcex = 1
)
#4472C4