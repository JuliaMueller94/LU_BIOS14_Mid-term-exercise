###Step 1: Import data set "Alpine Plants" via "Import Dataset"; Console shows: > alpineplants_csv <- read.csv("~/LU/BIOS14_Statistics/alpineplants_csv.csv", sep=";")
###Step 2: Name data set, View & names
alpineplants <- alpineplants_csv
View(alpineplants)
names(alpineplants)

###Step 3: Plotting raw data 

#LIGHT#

plot(alpineplants$light, alpineplants$Carex.bigelowii,
     las=1,
     main= "Impact of light on species distribution",
     ylab= "Abundance of plants [numbers]",
     xlab= "Light",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.7,
     cex = 1.5)
points(alpineplants$light, alpineplants$Thalictrum.alpinum, col= "darkolivegreen", pch = 18, cex=1.7)
legend("topright", col=c(1,"darkolivegreen"), pch=21:18, cex = 1.4,
       legend=c(expression(paste(italic("Carex bigelowii"))),
                expression(paste(italic("Thalictrum alpinum")))))

#SUMMER-MEAN-TEMPERATURE#

plot(alpineplants$mean_T_summer, alpineplants$Carex.bigelowii,
     las=1,
     main="Impact of mean summer temperature on species distribution",
     ylab= "Abundance of plants [numbers]",
     xlab= "Mean summer temperature [?C]",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.7,
     cex= 1.5)
points(alpineplants$mean_T_summer, alpineplants$Thalictrum.alpinum, col="darkolivegreen", pch= 18, cex=1.7)
legend("topleft",col=c(1,"darkolivegreen"), pch=21:18, cex = 1.4,
       legend=c(expression(paste(italic("Carex bigelowii"))),
                expression(paste(italic("Thalictrum alpinum")))))

#ALTITUDE#

plot(alpineplants$altitude, alpineplants$Carex.bigelowii,
     las=1,
     main="Impact of altitude on species distribution",
     ylab= "Abundance of plants [numbers]",
     xlab= "Altitude [m]",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.7,
     cex= 1.5)
points(alpineplants$altitude, alpineplants$Thalictrum.alpinum, col="darkolivegreen", pch= 18, cex=1.7)
legend("topleft",col=c(1,"darkolivegreen"), pch=21:18, cex = 1.4,
       legend=c(expression(paste(italic("Carex bigelowii"))),
                expression(paste(italic("Thalictrum alpinum")))))

#ALTITUDE AND TEMPERATURE#

plot(alpineplants$altitude, alpineplants$mean_T_summer,
     las=1,
     main="Correlation of altitude and temperature",
     ylab="Mean summer temperature [?C]",
     xlab="Altitude [m]",
     cex.lab = 1.3,
     cex.axis = 1.2,
     cex.main = 1.7,
     cex= 1.5)
abline(lm(alpineplants$mean_T_summer~alpineplants$altitude), col="darkorange3", lwd=2)

#ALTITUDE AND LIGHT#

plot(alpineplants$altitude, alpineplants$light,
     las=1,
     main="Correlation of altitude and light",
     ylab="Light",
     xlab="Altitude [m]",
     cex.lab = 1.3,
     cex.axis = 1.2,
     cex.main = 1.7,
     cex= 1.5)
abline(lm(alpineplants$light~alpineplants$altitude), col="darkorange3", lwd=2)

#TEMPERATURE AND LIGHT#

plot(alpineplants$mean_T_summer, alpineplants$light,
     las=1,
     main="Correlation of temperature and light",
     ylab="Light",
     xlab="Mean summer temperature [?C]",
     cex.lab = 1.3,
     cex.axis = 1.2,
     cex.main = 1.7,
     cex= 1.5)
abline(lm(alpineplants$light~alpineplants$mean_T_summer), col="darkorange3", lwd=2)

###Step 4: Model selection###
#1. Design of the models 
m1 = lm(alpineplants$Thalictrum.alpinum~alpineplants$mean_T_summer)
m2 = lm(alpineplants$Thalictrum.alpinum~alpineplants$light)
m3 = lm(alpineplants$Thalictrum.alpinum~alpineplants$altitude)
m4 = lm(alpineplants$Thalictrum.alpinum~alpineplants$mean_T_summer*alpineplants$light*alpineplants$altitude)
m5 = lm(alpineplants$Thalictrum.alpinum~alpineplants$mean_T_summer+alpineplants$light+alpineplants$altitude)

#2. Creating the table to compare the models
mlist = list(m1, m2, m3, m4, m5)
AICTab = AIC(m1, m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#RESULTS FOR MODEL SELECTION#

# df      AIC    logLik    delta   w
# m4  9 399.2180 -190.6090  0.00 0.59
# m5  5 400.4919 -195.2459  1.27 0.31
# m1  3 403.1341 -198.5671  3.92 0.08
# m2  3 407.9381 -200.9690  8.72 0.01
# m3  3 415.6922 -204.8461 16.47 0.00

# based on the models tested in this step, model m4 shows the lowest AIC value (399.2180) and the highest weight (w = 0.59)
# BUT: m4 is also a more complex model (df=9); the AIC of the 2nd best model m5 doesn't differ too much (400.4919)
# weight for m5 is also okay ( w = 0.31) and it's delta value is closer to 2 (1.27)
# When two models have a good fit, one can also go for the one with higher AIC and lower weight, when it is e.g. less complex which can make it easier to analyze regarding to a specific question
# Hence, the choice for this analysis is the multiple regression model (m5)

####Step 5: Multiple regression based on z-transformation pf predictors

#x1_z = (x1 - mean(x1))/sd(x1)
#x2_z = (x2 - mean(x2))/sd(x2)
#m = lm(y ~ x1_z + x2_z)
#summary(m)

Temperature_z = (alpineplants$mean_T_summer-mean(alpineplants$mean_T_summer, na.rm = T))/sd(alpineplants$mean_T_summer, na.rm = T)
#na.rm = ; fixes mistakes/numbers
Altitude_z = (alpineplants$altitude-mean(alpineplants$altitude))/sd(alpineplants$altitude)

Light_z = (alpineplants$light-mean(alpineplants$light))/sd(alpineplants$light)

mC_z = lm(alpineplants$Carex.bigelowii~Temperature_z+Altitude_z+Light_z) #mC_z = Carex

summary(mC_z)

# Call:
#   lm(formula = alpineplants$Carex.bigelowii ~ Temperature_z + Altitude_z + Light_z)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4143 -1.1229 -0.5248  0.8781  5.2346 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.38114    0.16740   8.251 1.16e-12 ***
#   Temperature_z  0.99572    0.32570   3.057  0.00293 ** 
#   Altitude_z     1.29389    0.28995   4.462 2.31e-05 ***
#   Light_z       -0.05185    0.22068  -0.235  0.81477    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.631 on 91 degrees of freedom
# (1 Beobachtung als fehlend gel?scht)
# Multiple R-squared:  0.1815,	Adjusted R-squared:  0.1545 
# F-statistic: 6.726 on 3 and 91 DF,  p-value: 0.0003773

mT_z = lm(alpineplants$Thalictrum.alpinum~Temperature_z+Altitude_z+Light_z) #mT_z = Thalictrum

summary(mT_z)

# Call:
#   lm(formula = alpineplants$Thalictrum.alpinum ~ Temperature_z + 
#        Altitude_z + Light_z)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7666 -1.1004 -0.4558  0.2678  7.5506 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     1.2430     0.1982   6.273 1.17e-08 ***
#   Temperature_z   1.1027     0.3855   2.860  0.00525 ** 
#   Altitude_z      0.7587     0.3432   2.211  0.02956 *  
#   Light_z         0.3193     0.2612   1.222  0.22473    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.931 on 91 degrees of freedom
# (1 Beobachtung als fehlend gel?scht)
# Multiple R-squared:  0.1706,	Adjusted R-squared:  0.1433 
# F-statistic:  6.24 on 3 and 91 DF,  p-value: 0.0006689

mAT_z = lm(Temperature_z~Altitude_z) #mAT_z = Altitude ~ Temperature
summary(mAT_z)

# Call:
#   lm(formula = Temperature_z ~ Altitude_z)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.26028 -0.36734 -0.06252  0.34475  1.52074 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.01093    0.06009  -0.182    0.856    
# Altitude_z  -0.81558    0.06061 -13.456   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5857 on 93 degrees of freedom
# (1 Beobachtung als fehlend gel?scht)
# Multiple R-squared:  0.6607,	Adjusted R-squared:  0.657 
# F-statistic: 181.1 on 1 and 93 DF,  p-value: < 2.2e-16


mTL_z = lm(Light_z~Temperature_z) #mTL_z = Temperature ~ Light
summary (mTL_z)

# Call:
#   lm(formula = Light_z ~ Temperature_z)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.67528 -0.60848 -0.05998  0.51639  1.70679 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.01244    0.07868   0.158    0.875    
# Temperature_z  0.64327    0.07910   8.133 1.79e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7669 on 93 degrees of freedom
# (1 Beobachtung als fehlend gel?scht)
# Multiple R-squared:  0.4156,	Adjusted R-squared:  0.4093 
# F-statistic: 66.14 on 1 and 93 DF,  p-value: 1.788e-12

mAL_z = lm(Altitude_z~Light_z) #mAL_z = Altitude ~ Temperature
summary (mAL_z)

# Call:
#   lm(formula = Altitude_z ~ Light_z)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.17697 -0.82564 -0.02557  0.74344  1.41739 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.157e-15  8.793e-02   0.000        1    
# Light_z     -5.154e-01  8.839e-02  -5.831 7.73e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8615 on 94 degrees of freedom
# Multiple R-squared:  0.2656,	Adjusted R-squared:  0.2578 
# F-statistic:    34 on 1 and 94 DF,  p-value: 7.732e-08