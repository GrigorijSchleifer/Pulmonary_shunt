y <- read.table(pipe("pbpaste"))
rm(list = ls()) 

i <- 1

value <- matrix(
  c(rep(0, 8)),
  dimnames = list(c("Hb", "fO2art", "fO2mven", "PaO2", 
               "PaCO2", "PvO2", "fiO2", "Patm")))


ask_for_values <- function(blood_values){
  while(i <= length(blood_values)){
    input <- NA
    while(is.na(input) == TRUE){
      input <- as.numeric(readline(cat(names(blood_values[i,]), "is: ")))
    }
      blood_values[i,] <- input
      i <- i+1
  }
  # shunt_to_print <- shunt_calc(blood_values)
  # str(shunt_to_print)
  # cut("The shunt is:", shunt_to_print)
}
 

shunt_calc <- function(value) {
  # partial alveolar O2 pressure
  # FiO2*(Patm-PH2O)-PaCO2/RQ + Correction Factor (2)
  PAO2 <- as.numeric((value["Patm",]-47)-(value["PaCO2",]/0.8)+2)
    
  # CAPILLARY OXYGEN CONTENT
  # CcO2 = (Hgb * 1.31) + (0.0031 * PAO2) 
  # no multiplication with the arterial sat fraction
  Cco2 <- as.numeric(value["Hb",]*1.31)+(0.0031*PAO2)
  
  # ARTERIAL OXYGEN CONTENT
  Cao2 <- as.numeric(value["Hb",]*1.31*(value["fO2art",]/100))+(0.0031*value["PaO2",])
  
  # MIXED VEONOUS OXYGEN CONTENT
  Cvo2 <- as.numeric(value["Hb",]*1.31*(value["fO2mven",]/100))+(0.0031*value["PvO2",])
  
  # SHUNT
  SHUNT <- ((Cco2-Cao2)/(Cco2-Cvo2))*100
  return(SHUNT)
}


# dont forget to assign the method return to a new object
# otherwise everything stays inside the method
blood_gas <- ask_for_values(value)
S <- shunt_calc(blood_gas)





