# Soils without and with lime
twosamp <- read.table("day_1/data/lime2.dat", col.names=c("lime", "ph"))
# reads a raw ASCII datafile; column names are specified 
# result is dataframe named "twosamp"
# two groups: 5 soils without lime, 5 soils with lime

# make dummy variables:
(x1 <- as.numeric(twosamp$lime=="N"))
(x2 <- as.numeric(twosamp$lime=="Y"))

# "-1" removes the intercept from model
lmo1 <- lm(ph ~ -1 + x1 + x2, data=twosamp)
coef(lmo1)

lmo2 <- lm(ph ~ x1, data=twosamp )
coef(lmo2)

