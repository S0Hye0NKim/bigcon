#User-defined function for Big-Contest 2019

Classify_Wrn_Day <- function(x) {
  if("Dust_Watch" %in% x) {
    return("Dust_Watch")
  } else if("Warning" %in% x) {
    return("Warning")
  } else {return("No_Wrn")}
}



Mode <- function(x) {
  table(x) %>% which.max() %>% names()
}


Classify_Dust_lev <- function(pm, tiny) {
  pm <- as.numeric(pm)
  if(tiny == 10) {
    pm <- case_when((pm <= 30) ~ "Good", 
                    (pm >30 & pm <=50) ~ "Moderate", 
                    (pm >50 & pm <=75) ~ "Sens_Unhealthy", 
                    (pm > 75 & pm <=100) ~ "Unhealthy", 
                    (pm > 100 & pm <= 150) ~ "Very Unhealthy", 
                    (pm > 150) ~ "Worst")
  } else if (tiny == 25) {
    pm <- case_when((pm <= 15) ~ "Good", 
                    (pm>15 & pm<=25) ~ "Moderate", 
                    (pm>25 & pm<=37) ~ "Sens_Unhealthy", 
                    (pm>37 & pm<=50) ~ "Unhealthy", 
                    (pm>50 & pm<=75) ~ "Very Unhealthy", 
                    (pm>75) ~ "Worst")
  }
  Dust_lev <- c("Good", "Moderate", "Sens_Unhealthy", "Unhealthy", "Very Unhealthy", "Worst")
  pm <- factor(pm, levels = Dust_lev)
  return(pm)
}











