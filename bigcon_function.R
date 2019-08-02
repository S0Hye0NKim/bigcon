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