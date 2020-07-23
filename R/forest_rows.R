forest_rows <- function(head_row, ord_var, index,
                        gap = 0, row_offset = 4, stitle_row_offset = 2){
   
   breaks <- c(sum(ord_var == "Early"),
               sum(ord_var == "Early") + sum(ord_var == "Late"),
               sum(ord_var == "Early") + sum(ord_var == "Late") + 
                  sum(ord_var == "Recommended"),
               sum(ord_var == "Early") + sum(ord_var == "Late") + 
                  sum(ord_var == "Recommended") + sum(ord_var == "Recommended_plus"),
               sum(ord_var == "Early") + sum(ord_var == "Late") + 
                  sum(ord_var == "Recommended") + sum(ord_var == "Recommended_plus") + sum(ord_var == "Late_plus"),
               sum(ord_var == "Early") + sum(ord_var == "Late") + 
                  sum(ord_var == "Recommended") + sum(ord_var == "Recommended_plus") + sum(ord_var == "Late_plus") +
                  sum(ord_var == "control"))
   
   trows_F <- function(x1, head_row, gap){
      if(x1 <= length(ord_var) &&
         x1 > length(ord_var) - (breaks[1])){
         return(x1 + (head_row - length(ord_var)))}
      
      if(x1 <= length(ord_var) - (breaks[1]) &&
         x1 > length(ord_var) - 
         (breaks[2])){
         return(x1 + (head_row - length(ord_var)) - (gap))}
      
      if(x1 <= length(ord_var) - breaks[2] &&
         x1 > length(ord_var) - breaks[3]){
         return(x1 + (head_row - length(ord_var)) - (gap * 2))}
      
      if(x1 <= length(ord_var) - breaks[3] &&
         x1 > length(ord_var) - breaks[4]){
         return(x1 + (head_row - length(ord_var)) - (gap * 3))}
      
      if(x1 <= length(ord_var) - breaks[4] &&
         x1 > length(ord_var) - breaks[5]){
         return(x1 + (head_row - length(ord_var)) - (gap * 4))}
      
      if(x1 <= length(ord_var) - breaks[5] &&
         x1 > length(ord_var) - breaks[6]){
         return(x1 + (head_row - length(ord_var)) - (gap * 5))}
      
   }
   
   stitle_row <- head_row - c(stitle_row_offset,
                              stitle_row_offset + breaks[1] + gap,
                              stitle_row_offset + breaks[2] + (gap*2),
                              stitle_row_offset + breaks[3] + (gap*3),
                              stitle_row_offset + breaks[4] + (gap*4),
                              stitle_row_offset + breaks[5] + (gap*5))
   
   
   dat2 <- list(stitle_row = stitle_row,
                
                pred_row = head_row - c(stitle_row_offset + breaks[1],
                                        stitle_row_offset + breaks[2] + gap,
                                        stitle_row_offset + breaks[3] + (gap*2),
                                        stitle_row_offset + breaks[4] + (gap*3),
                                        stitle_row_offset + breaks[5] + (gap*4),
                                        stitle_row_offset + breaks[6] + (gap*5)),
                
                trows = unlist(sapply(index,trows_F, head_row = head_row, gap = gap)) + row_offset,
                
                breaks = breaks
                )
   
   dat2
}
