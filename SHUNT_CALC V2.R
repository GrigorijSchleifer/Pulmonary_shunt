y <- read.table(pipe("pbpaste"))
rm(list = ls()) 

i <- 1

blood_values <- matrix(
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
  return(blood_values)
}

# dont forget to assign the method return to a new object
# otherwise everything stays inside the method
blood_gas <- ask_for_values(blood_values)


# partial alveolar O2 pressure
# FiO2*(Patm-PH2O)-PaCO2/RQ + Correction Factor (2)
PAO2 <- as.numeric(blood_gas["fiO2",]*(blood_gas["Patm",]-46)-(blood_gas["PaCO2",]/0.8))
  
# capillary oxygen content
# CcO2 = (Hgb * 1.31) + (0.0031 * PAO2) 
# no multiplication with the arterial sat fraction
Cco2 <- as.numeric(blood_gas["Hb",]*1.39)+(0.0031*PAO2)


# mixed venous oxygen content
Cvo2 <- as.numeric(blood_gas["Hb",]*1.39*(blood_gas["fO2art",]/100))+(0.0031*PAO2)






runif(100, 0,1) # homogenious destributer
rnorm(100, 0,1) # normal, bell destributed
rbinom(100, 5, .9) # "prob" argument 90% of 1 to 10% 0, also binomial
?rbinom


table(x)
xy <- rbinom(100, 1, .6)
table(xy)



ggplot(x, aes(a)) +
  geom_histogram(binwidth = .1)+
  geom_vline(aes(xintercept = low, col ="red"))+
  geom_vline(aes(xintercept = high, col ="red"))






