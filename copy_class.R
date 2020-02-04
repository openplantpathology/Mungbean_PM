copy_class <- function(template_data, incompatible_class_data){

#Data Checks
   if(any(colnames(template_data) != colnames(incompatible_class_data))){
      warning("columns don't match ")
      stop()}
   if(missing(template_data)){warning("\nTemplate data set needed ")
      stop()}
   if(missing(incompatible_class_data)){
      warning("\nincompatible_class_data set needed ")
      stop()}
   if(missing(incompatible_class_data)){
      warning("\nplease specifiy column name ")
      stop()}
   
   template_data <- data.frame(template_data, stringsAsFactors = FALSE)
   incompatible_class_data <- as.data.frame(incompatible_class_data, stringsAsFactors = FALSE)
   
   
   for(i in colnames(template_data)){
      col_class <- class(template_data[,i])
      if(col_class == "factor"){
         incompatible_class_data[,i] <- as.factor(incompatible_class_data[,i])
      }else{
         class(incompatible_class_data[,i]) <- col_class   
      }
   }

   
      
   return(incompatible_class_data)
}