
# Searches a dataframe (template_data) based on all columns which are not "ignored".
#     at the first match the function inserts data entry from the "new_data_column in the 
#     dataframe new_data into the column with the same name in template_data.
# returns a dataframe

replace_data <- function(template_data, new_data, new_data_column, ignore_columns = NULL, trial_ref = NULL){
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
   
   
   
   
   if(is.null(trial_ref) == FALSE){
      NMC <- vector()
     if(any(trial_ref %in% unique(template_data$trial_ref))){
        
        dat3 <- template_data[template_data$trial_ref == unique(new_data$trial_ref),]
        
        for(i2 in 1:length(colnames(template_data))){
           message(colnames(template_data)[i2], "match = ",
              unique(template_data[template_data$trial_ref == trial_ref,1]) != unique(King_11_m[,1])
           )
           
        }
        
        
        # for(i1 in colnames(dat3)){
        #    if(class(dat3[,i1]) == "factor"){
        #       dat3[,i1] <- as.character(dat3[,i1])
        #    }
        #    
        #    if(length(unique(dat3[,i1])) != length(unique(new_data[,i1]))){
        #       NMC <- c(NMC,i1)
        #    }else{
        #       if(any(unique(dat3[,i1]) != unique(new_data[,i1]))){
        #          NMC <- c(NMC,i1)
        #       }}
        #    }
        # return(NMC)
     stop()
        }
      
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
      
      
       
   }
   
   
   
   for(i in 1:dim(dat1)[1]){
      for(j in 1:dim(dat2)[1]){
   
               
         if(any(is.na(dat1[i,]) != is.na(dat2[j,])) == FALSE){
            if(all(dat1[i,!is.na(dat1[i,])] == dat2[j,!is.na(dat2[j,])])){
               
               matched_rows <- c(matched_rows,i)
               
               if(length(unique(matched_rows)) != length(matched_rows)){
                  stop("\nError: matching row duplicate at template row",i,
                       "\n previous matched rows:", matched_rows)
               }
               
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

   if(is.null(trial_ref) == FALSE){
      
   }
      
      
   if(length(report_rows)==0){message("No data changed ", 
                                      length(report_rows),"/",length(matched_rows))}else{
      message("\nRows from template data replaced\n",paste(report_rows, collapse = ", "),
              "\ntotal replacements = ",length(report_rows),"/",length(matched_rows))}
   if(dim(new_data)[1] != length(matched_rows)){warning("\n\n NEW_DATA AND MATCHED ROWS ARE DIFFERENT DIMENSIONS\n",
                                                "Matched rows =",length(matched_rows),
                                                "\nnew_data rows =",dim(new_data)[1],
                                                "\nCHECK IGNORED COLUMNS TO ENSURE MATCHES ARE CORRECT!!!!\n")}
return(template_data)
   }


