
# Searches a dataframe (template_data) based on all columns which are not "ignored".
#     at the first match the function inserts data entry from the "new_data_column in the 
#     dataframe new_data into the column with the same name in template_data.
# returns a dataframe

replace_data <- function(template_data, new_data, new_data_column, ignore_columns = NULL, filter_cols = NULL,subset_factor = NULL){
   if(any(colnames(template_data) != colnames(new_data))){warning("columns don't match ")
      stop()}
   
   if(missing(template_data)){warning("\nTemplate data set needed ")
      stop()}
   if(missing(new_data)){warning("\nnew_data set needed ")
      stop()}
   if(missing(new_data_column)){warning("\nplease specifiy column name ")
      stop()}
   
   template_data <- data.frame(template_data, stringsAsFactors = FALSE)
   new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)
   
   if(any(!new_data_column %in% colnames(template_data))){warning("\nCan't find index column in template dataset ")
      stop()}
   
   if(any(colnames(template_data) != colnames(new_data))){warning("\nColumns from data don't match")
      stop()}
   if(all(!is.null(ignore_columns) & !is.null(filter_cols))){warning("\nSpecify only one criteria, not both filter_cols and ignore_columns ")
      stop()}
   
   if(is.numeric(ignore_columns)){
      ignore_columns <- colnames(template_data)[ignore_columns]
   }
   if(!is.null(ignore_columns)){
      if(class(ignore_columns) != "character"){warning("\nignore_columns not a character vector ")
         
      }
      if(any(colnames(template_data) %in% ignore_columns) == FALSE){
         message(paste("\nNone of the specified columns",ignore_columns,"are included in the template_data "))
      }
      
      #specify the columns to match
      cols1 <- colnames(template_data)[colnames(template_data) %in% c(new_data_column,ignore_columns)]
      message("\nExcluding columns from match: \n",paste(cols1, collapse = ", "))
      
   }else{# when ignore_columns is NULL
         if(is.numeric(filter_cols)){
            filter_cols <- colnames(template_data)[filter_cols]
            
            #specify the columns to match
            message("\n Matching by columns: \n",paste(filter_cols, collapse = ", "))
            
               }
      cols1 <- colnames(template_data)[!(colnames(template_data) %in% filter_cols)] # Removes the columns from the template_data set and return all column names except the filter_cols
      }
   
   
   
   
   # subset datasets to give the columns to filter by
   dat1 <- template_data[,!colnames(template_data) %in% cols1] 
   dat2 <- new_data[,!colnames(new_data) %in% cols1] 
   
   # Create empty vectors
   report_rows <- vector(mode = "numeric")
   matched_rows <- vector(mode = "numeric")
   index_row <- 0 
   message1 <- 0
   

   
   
   
   
   if(is.null(subset_factor) == FALSE){ # this functionality has not been complete, it seems it is an attempt to speed up the function by using the subset_factor to filter the match
      # To fix this it will need two arguments 1 to specify the column, 2 to specify the factor
      
      for(oo in colnames(template_data)){
         if(oo == colnames(template_data)[1]){
            factor_column <- vector()
         } # Reset to empty vector on the first loop
         factor_column <- c(factor_column,any(template_data[,oo] == subset_factor))
      } # find column subset_factor matches to
      
      if(sum(factor_column) >= 2){
         stop("subset_factor is present in multiple columns, functionality does not support subsetting by more than one factor")
      }
      if(sum(factor_column) == 0){
         stop("subset_factor could not be matched in template data")
      }
      if(sum(factor_column) == 0){
         stop("subset_factor could not be matched in template data")
      }
      if(!factor_column %in% colnames(new_data)){
         stop("subset_factor is in template_data column which is not in new_data")
      }
      if(!subset_factor %in% new_data[,factor_column]){
         stop(paste("subset_factor is can't be found in new_data column", factor_column))
      }
      
      subset_col <- colnames(template_data)[factor_column]
      subset_rows <- template_data[,factor_column] == subset_factor
      
      if(nrow(template_data[subset_rows,]) != nrow(new_data)){
         stop(paste0("Subsetted template_data has different number of rows (", 
                     nrow(template_data[subset_rows,]),
                     ") compared to new_data (",nrow(new_data),")"))
      }
      
   
      for(i in which(subset_rows)){
         for(j in 1:nrow(dat2)){
            
            
            if(any(is.na(dat1[i,]) != is.na(dat2[j,])) == FALSE){ # Are there any NAs in the filter_by columns when the other data has values if so skip row
               if(all(dat1[i,!is.na(dat1[i,])] == dat2[j,!is.na(dat2[j,])])){ # if the filter_by rows (minus any matched NAs )match, record the matched row
                  
                  matched_rows <- c(matched_rows, i)
                  
                  if(length(unique(matched_rows)) != length(matched_rows)){
                     stop("\nError: matching row duplicate at template row",i,
                          "\n previous matched rows:", matched_rows)
                  }
                  
                  if(anyNA(template_data[i, new_data_column]) & 
                     anyNA(new_data[j, new_data_column])== FALSE){
                     
                     template_data[i,new_data_column] <- new_data[j,new_data_column]
                     
                     report_rows <- c(report_rows,i)   
                     message("Template_data NA values were replaced by non-NA values from new_data")
                     # if the template replacement rows contain NAs when the new_data does not. Replace the template rows with new data.
                  next()
                     }
                  
                  if(all(template_data[i, new_data_column] != new_data[j, new_data_column]) |
                     is.na(all(template_data[i, new_data_column] == new_data[j, new_data_column])) &
                     anyNA(template_data[i, new_data_column]) &
                     anyNA(new_data[j, new_data_column]) == FALSE){# If template data and new_data from the columns to be matched are the same go to the next new_data row
                  
                  template_data[i,new_data_column] <- new_data[j,new_data_column]
                  
                  report_rows <- c(report_rows,i)
                  # message("working")
               
               }else{
                  next()}
            }else{
               next()}
            }else{
               next()}
         }
      } # Replacement code if data subset
      
      
       
   } 
   
   
   
   for(i in 1:nrow(dat1)){
      for(j in 1:nrow(dat2)){
   
               
         if(any(is.na(dat1[i,]) != is.na(dat2[j,])) == FALSE){
            if(all(dat1[i,!is.na(dat1[i,])] == dat2[j,!is.na(dat2[j,])])){
               
               matched_rows <- c(matched_rows,i)
               
               if(length(unique(matched_rows)) != length(matched_rows)){
                  stop("\nError: matching row duplicate at template row",i,
                       "\n previous matched rows:", matched_rows)
               }
               
               if(anyNA(template_data[i, new_data_column]) & 
                  anyNA(new_data[j, new_data_column])== FALSE){
                  
                  template_data[i,new_data_column] <- new_data[j,new_data_column]
                  
                  report_rows <- c(report_rows,i)   
                  message1 <- c(message1,1)
                  # if the template replacement rows contain NAs when the new_data does not. Replace the template rows with new data.
                  next()
               }
               
               if(is.na(all(template_data[i, new_data_column] == new_data[j, new_data_column])) &
                  anyNA(template_data[i, new_data_column]) &
                  anyNA(new_data[j, new_data_column]) == FALSE |
                  all(template_data[i, new_data_column] != new_data[j, new_data_column])){
               template_data[i,new_data_column] <- new_data[j,new_data_column]
               
               report_rows <- c(report_rows,i)
               #message("working")
      
               }else{
                  next()}
            }else{
               next()}
         }else{
            next()}
      }
   }

   
   if(length(report_rows)==0){message("No data changed ", 
                                      length(report_rows),"/",length(matched_rows))}else{
      message("\nRow number/s from template data replaced: c(",paste(report_rows, collapse = ", "),")",
              "\ntotal replacements = ",length(report_rows),"/",length(matched_rows))}
   if(dim(new_data)[1] != length(matched_rows)){warning("\n\n NEW_DATA AND MATCHED ROWS ARE DIFFERENT DIMENSIONS\n",
                                                "Matched rows =",length(matched_rows),
                                                "\nnew_data rows =",dim(new_data)[1],
                                                "\nCHECK IGNORED COLUMNS TO ENSURE MATCHES ARE CORRECT!!!!\n")}
   if(sum(message1) >= 1){message(paste(sum(message1)," NA values in template_data were replaced by non-NA values from new_data"))}
return(template_data)
   }


