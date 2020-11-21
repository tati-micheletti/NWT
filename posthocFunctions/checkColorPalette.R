# Check the pallete
checkColorPalette <- function(pal){
  plot(NULL, xlim=c(0,length(pal)), ylim=c(0,1),
       xlab="", ylab="", xaxt="n", yaxt="n")
  rect(0:(length(pal)-1), 0, 1:length(pal), 1, col=pal) 
}
