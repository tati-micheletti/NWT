firePosthocPlots <- function(df = df){
  reproducible::Require("ggplot2")
  
  breaks = seq(from = min(df$pSpread), to = max(df$pSpread), length.out = 6)
  
  meanFireSize <- ggplot(df, aes(x = prdMeanSize, y = modMeanSize, color = pSpread)) +
    geom_point(aes(prdMeanSize, modMeanSize)) + 
    scale_color_gradient(low = "blue", high = "red")+
    labs(y = "predicted size", x = "empirical size") +
    theme_minimal() + 
    geom_abline(slope = 1) +
    geom_text(aes(label = PolyId), hjust = -0.4, vjust = 0)
  
  
  meanAnnualAreaBurned <- ggplot(df, aes(x = hist_MAAB, y = mod_MAAB, col = pSpread)) +
    geom_point(aes(hist_MAAB, mod_MAAB)) +
    labs(y = "model mean annual area burned (%)", x = "empirical mean annual area burned (%)") +
    scale_color_gradient(low = "blue", high = "red")+
    theme_minimal() +
    geom_abline(slope = 1) +
    geom_text(aes(label = PolyId), hjust = -0.4, vjust = 0)
  
  return(list(meanFireSize = meanFireSize, meanAnnualAreaBurned = meanAnnualAreaBurned))
}