reportP <- function(p,
                    AsNumeric = FALSE,
                    P_prefix = TRUE) {
   if (AsNumeric == TRUE &
       P_prefix == TRUE) {
      warning("'AsNumeric' and 'P_prefix' can't both be 'TRUE'.\n
           Forcing `P_prefix = FALSE`")
      P_prefix <- FALSE
      # normally I would make this a stop()
      # but I want to make the lastest version of this function backwards compatible
   }
   
   if (length(p) == 1) {
      if (AsNumeric == FALSE &
          P_prefix == TRUE) {
         if (p < 0.0001) {
            return("P < 0.0001")
         } else{
            return(paste("P =", format(
               round(p, digits =  4), scientific = FALSE
            )))
         }
      }
      if (AsNumeric == FALSE &
          P_prefix == FALSE) {
         if (p < 0.0001) {
            return("< 0.0001")
         } else{
            return(format(round(p, digits =  4), scientific = FALSE))
         }
      }
      
      if (AsNumeric == TRUE) {
         if (p < 0.0001) {
            return(format(0.0001, scientific = FALSE))
         } else{
            return(format(round(p, digits =  4), scientific = FALSE))
         }
      }
   } else{
      sapply(p, function(p) {
         if (AsNumeric == FALSE &
             P_prefix == TRUE) {
            if (p < 0.0001) {
               return("P < 0.0001")
            } else{
               return(paste("P =", format(
                  round(p, digits =  4), scientific = FALSE
               )))
            }
         }
         
         if (AsNumeric == FALSE &
             P_prefix == FALSE) {
            if (p < 0.0001) {
               return("< 0.0001")
            } else{
               return(format(round(p, digits =  4), scientific = FALSE))
            }
         }
         
         if (AsNumeric == TRUE) {
            if (p < 0.0001) {
               return(format(0.0001, scientific = FALSE))
            } else{
               return(format(round(p, digits =  4), scientific = FALSE))
            }
         }
      })
      
   }
   
}