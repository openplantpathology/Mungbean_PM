reportP <- function(p){
   if(p < 0.0001){
      return("P < 0.0001")
   }else{
      return(paste("P =", format(round(p, digits =  4), scientific = FALSE)))
   }
}