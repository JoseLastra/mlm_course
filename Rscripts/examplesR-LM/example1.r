# Simple linear regression of wgtgain on clover consumption for guinea pigs
                                                                           
setwd("M:/LM/data")  # Change to whatever path you use yourself
guineapig <- read.csv("guineapigsSteelTorrie.csv")    # This reads in a comma delimited file

guineapig


reg.result <- lm(wgtgain ~  consump, data=guineapig)

plot(wgtgain ~ consump, data=guineapig)
abline(reg.result)
