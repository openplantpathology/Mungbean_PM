
# Searches a dataframe (template_data) based on all columns which are not "ignored".
#     at the first match the function inserts data entry from the "new_data_column in the 
#     dataframe new_data into the column with the same name in template_data.
# returns a dataframe

replace_data <- function(template_data, new_data, new_data_column, ignore_columns = NULL){
   if(any(colnames(template_data) != colnames(new_data))){warning("columns don't match ")
      stop()}
   
   if(missing(template_data)){warning("\nTemplate data set needed ")
      stop()}
   if(missing(new_data)){warning("\nnew_data set needed ")
      stop()}
   if(missing(new_data_column)){warning("\nplease specifiy column name ")
      stop()}
   
   template_data <- as.data.frame(template_data)
   new_data <- as.data.frame(new_data)
   
   if(all(new_data_column != colnames(template_data))){warning("\nCan't find index column in template dataset ")
      stop()}
   if(is.numeric(ignore_columns)){
      ignore_columns <- colnames(template_data)[ignore_columns]
   }
   
   if(any(colnames(template_data) != colnames(new_data))){warning("\nColumns from data don't match")
      stop()}
   
   if(!is.null(ignore_columns)){
      if(class(ignore_columns) != "character"){warning("\nignore_columns not a character vector ")
         
      }
      if(any(colnames(template_data) %in% ignore_columns) == FALSE){
         message(paste("\nNone of the specified columns",ignore_columns,"are included in the template_data "))
      }   
     
      }
   
   #specify the columns to match
   cols1 <- colnames(template_data)[colnames(template_data) %in% c(new_data_column,ignore_columns)]
   message("\nExcluding columns from match: \n",paste(cols1, collapse = ", "))
   
   
   # subset datasets
   dat1 <- template_data[,!colnames(template_data) %in% cols1]
   dat2 <- new_data[,!colnames(new_data) %in% cols1]
   
   #iterator1 <- 1
   report_rows <- vector(mode = "numeric")
   matched_rows <- vector(mode = "numeric")
   
   for(i in 1:dim(dat1)[1]){
      for(j in 1:dim(dat2)[1]){
   
               
         if(any(is.na(dat1[i,]) != is.na(dat2[j,])) == FALSE){
            if(all(dat1[i,!is.na(dat1[i,])] == dat2[j,!is.na(dat2[j,])])){
               
               matched_rows <- c(matched_rows,i)
               
               
               if(template_data[i,new_data_column] == new_data[j,new_data_column]){next()}
               template_data[i,new_data_column] <- new_data[j,new_data_column]
               
               report_rows <- c(report_rows,i)
               #message("working")
         
               }else{
                  next()}
            }else{
               next()}
      }
   }
   
      
   if(length(report_rows)==0){message("No data changed ", 
                                      length(report_rows),"/",length(matched_rows))}else{
      message("\nRows from template data replaced\n",paste(report_rows, collapse = ", "),
              "\ntotal replacements = ",length(report_rows),"/",length(matched_rows))}
   
return(template_data)
   }


