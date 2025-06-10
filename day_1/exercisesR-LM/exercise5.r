setwd("C:/MyData/OneDrive - WageningenUR/Onderwijs/PhD-courses/AdvancedStatisticsModuleLM/course2020_LM/data")  # Change to whatever path you use yourself

guineapig <- read.csv("guineapigsSteelTorrie.csv")    # This reads in a comma delimited file


selection <- guineapig$soiltype=="loam1" | guineapig$soiltype=="loam2"    # This logical expression returns true and false, indicating which observations to select
loamysoils <- guineapig[selection,]   
loamysoils$soiltype <- factor(loamysoils$soiltype) # make it a factor, i.e. grouping variable

# Actual selection
loamysoils

# Independent samples t-test
t.test(wgtgain ~ soiltype, data=loamysoils, var.equal=T)                  # Assume equal variances of wgtgain for the two groups
