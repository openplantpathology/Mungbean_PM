reportP <- function(p, AsNumeric = FALSE){
   if(length(p) == 1){
   
   if(AsNumeric == FALSE){
      if(p < 0.0001){
      return("P < 0.0001")
   }else{
      return(paste("P =", format(round(p, digits =  4), scientific = FALSE)))
   }}
   
   if(AsNumeric == TRUE){
      if(p < 0.0001){
      return(format(0.0001, scientific = FALSE))
   }else{
      return(format(round(p, digits =  4), scientific = FALSE))
   }}}else{
      
      sapply(p,function(p) {
         if(AsNumeric == FALSE){
            if(p < 0.0001){
               return("P < 0.0001")
            }else{
               return(paste("P =", format(round(p, digits =  4), scientific = FALSE)))
            }}
         
         if(AsNumeric == TRUE){
            if(p < 0.0001){
               return(format(0.0001, scientific = FALSE))
            }else{
               return(format(round(p, digits =  4), scientific = FALSE))
            }}
      })
      
   }
   
}