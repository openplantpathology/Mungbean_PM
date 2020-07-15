# This function adds the comparison names to the multiple comparisons Tukey test
# it requires the summary of a glht test on a model object

source(here::here("R/p_star.R"))

simple_summary <-
   function(sum_meta){
      names1 <- colnames(sum_meta[["model"]]$G)
      ind_names <- names(sum_meta[["test"]]$coefficients)
      
      rename_index <- function(index_name, char_name){
         #if(length(index_name) != length(char_name)){stop("index_name and char_name not the same length")}
         for(i in rev(seq_along(char_name))){
            if(i == rev(seq_along(char_name))[1]){new_name <- index_name}
            new_name <- gsub(as.character(i),char_name[i], new_name)
         }
         return(new_name)
      }
      
      data.frame(contrast = rename_index(index_name = ind_names, char_name = names1),
                 coefficients = sum_meta[["test"]]$coefficients,
                 pvals = sum_meta[["test"]]$pvalues,
                 sig = p_star(sum_meta[["test"]]$pvalues)
                                         )
         
   }



#### TESTING
# 
# summary(glht(PM_mv, linfct=cbind(contrMat(rep(1,6), type="Tukey"))), test=adjusted("none"))[["test"]]$pvalues
# colnames(summary(glht(PM_mv, linfct=cbind(contrMat(rep(1,6), type="Tukey"))), test=adjusted("none"))[["test"]]$coefficients)
simple_summary(summary(glht(PM_mv, linfct=cbind(contrMat(rep(1,6), type="Tukey"))), test=adjusted("none")))
