# Simple linear regression

setwd("M:/LM/data")  # Change to whatever path you use yourself
limedata <- read.table("lime.prn", header=TRUE)    # This reads in a raw ASCII datafile; column names are given on first line of datafile

# Select observations with type of lime = AL:
limeAL <- limedata[limedata$lime=="AL",] 
head(limeAL)

# Fit simple linear regression model:
limeAL.reg <- lm(pH ~ rate, data=limeAL)

# Get ANOVA table:
anova(limeAL.reg)
# Note that R does not give the (corrected) total sum of squares SST!

# For completeness: corrected total sum of squares = SST:
(SST <-sum((limeAL$pH - mean(limeAL$pH))^2) )

# Other summary results for the regression analysis:
summary(limeAL.reg)

# Some code to make an ANOVA table with bottom line showing corrected total sum of squares
aot <- matrix(nrow=3, ncol=5)
aot[1:2,] <- as.matrix(anova(limeAL.reg))
aot[3,]   <- as.matrix(anova(lm(pH ~ 1, data=limeAL)))
rownames(aot) <- c("Corrected Model","Error","Corrected Total")
colnames(aot) <- c("Df","Sum Sq","Mean Sq","F value","Pr(>F)")
print(aot, na.print="")
