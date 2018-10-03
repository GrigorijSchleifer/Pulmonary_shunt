# http://intensivecarenetwork.com/Calculators/Files/Pulmonaire.html
rm(list = ls())

data_from_excel <- as.matrix(read.table(pipe("pbpaste")))
fiO2 <- 1
Patm <- 760
 
blood_manual <- matrix(
  dimnames = list(c("PaCO2","PaO2","Hb","fO2art","fO2mven","PvO2")), #Hb is arterial
                   data_from_excel[c(2:5, 12, 11)])


shunt_calc <- function(value) {
  PAO2 <- as.numeric((fiO2*(Patm-47))-(value["PaCO2",]/0.8)+2)
  # CAPILLARY OXYGEN CONTENT
  # CcO2 = (Hgb * 1.31) + (0.0031 * PAO2) 
  # no multiplication with the arterial sat fraction
  Cco2 <- as.numeric(value["Hb",]*1.34)+(0.0031*PAO2)
  
  # ARTERIAL OXYGEN CONTENT
  Cao2 <- as.numeric(value["Hb",]*1.34*(value["fO2art",]/100))+(0.0031*value["PaO2",])
  
  # MIXED VEONOUS OXYGEN CONTENT
  Cvo2 <- as.numeric(value["Hb",]*1.34*(value["fO2mven",]/100))+(0.0031*value["PvO2",])
  
  # SHUNT
  SHUNT <- ((Cco2-Cao2)/(Cco2-Cvo2))*100
  # dont forget to assign the method return to a new object
  # otherwise everything stays inside the method
  
  return(cat("The shunt is:", as.numeric(SHUNT), "\n", "\n",
              "PAO2 is", PAO2, "\n",
              "Cco2 is", Cco2, "\n",
              "Cao2 is", Cao2, "\n",
              "Cvo2 is", Cvo2))
}


shunt_calc(blood_manual)

