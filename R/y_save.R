y_save <- function(est,
                   se,
                   g_price_ha = 1.2,
                   fun_price_ha = 4.18,
                   applications = 1,
                   application_cost = 2) {
   net_return <- c((est - (se * 2)) * g_price_ha,
                   (est + (se * 2)) * g_price_ha)
   gross_return <-
      net_return - (fun_price_ha * (applications + application_cost))
   
   return(gross_return)
   
}
