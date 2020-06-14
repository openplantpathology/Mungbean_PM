# This function tests two vectors for each respectivley placed element is the same
# This works like testing x1 == x2 with the difference being it handles NA values differently
# The function treats NAs a meaningful test and returns if it is the same.
# ie. same(NA,NA) returns TRUE; where NA == NA return NA
#     same(1, NA) returns FALSE; where 1 == NA returns NA 
# Reference: http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/

same <- function(v1,v2) {
   if((v1 >=2 | v1 >=2) &
      (length(v1) != length(v1))){
      stop("can't compare vectors with different lengths")
   }
   compareNAs <- (v1 == v2) | (is.na(v1) & is.na(v2))
   compareNAs[is.na(compareNAs)] <- FALSE
   return(compareNAs)
}
