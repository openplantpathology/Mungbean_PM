# function to extract date from the year
# format should be Universal time format
year2day <- function(date) {
   if (is.na(date) == TRUE) {
      return(NA)
   } else {
      return(as.numeric(as.Date(date) - as.Date(paste0(
         substr(date, start = 1, stop = 4), "-01-01"
      ))))
   }
}