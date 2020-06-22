# This function tests two vectors for each respectivley placed element is the same
# This works like testing x1 == x2 with the difference being it handles NA values differently
# The function treats NAs a meaningful test and returns if it is the same.
# ie. same(NA,NA) returns TRUE; where NA == NA return NA
#     same(1, NA) returns FALSE; where 1 == NA returns NA 

same <- function(x1, x2) {
   if (length(x1) == length(x2)) {
      return(apply(cbind(x1, x2), 1, function(xx) {
         if (is.na(xx[1]) &
             is.na(xx[2])) {
            return(TRUE)
         } else{
            if (is.na(xx[1]) |
                is.na(xx[2])) {
               return(FALSE)
            } else{
               return(xx[1] == xx[2])
            }
         }
         
      }))
   }
   
   if (length(x1) == 1 &
       length(x2) > 1) {
      return(sapply(x2, function(xx = x2) {
         if (is.na(xx) &
             is.na(x1)) {
            return(TRUE)
         } else{
            if (is.na(xx) |
                is.na(x1)) {
               return(FALSE)
            } else{
               return(x1 == xx)
            }
            
         }
      }))
      
   }
   
   if (length(x1) > 1 &
       length(x2) == 1) {
      return(sapply(x1, function(xx = x1) {
         if (is.na(xx) &
             is.na(x2)) {
            return(TRUE)
         } else{
            if (is.na(xx) |
                is.na(x2)) {
               return(FALSE)
            } else{
               return(x2 == xx)
            }
            
         }
      }))
   }else{stop("length of vectors must be the same length")}
   
   
}