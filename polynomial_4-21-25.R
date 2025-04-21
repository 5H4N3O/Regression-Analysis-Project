res = model_A$residuals

plot(Admission$GRE ,res)
plot(Admission$TOEFL ,res)
plot(Admission$LOR ,res)
plot(Admission$CGPA ,res)
plot(Admission$Research ,res)

# 'dipping' effect appears in several residual plots, most apparent in CGPA
# try starting with quadratic terms for GRE, TOEFL, and LOR in model and backward fit?

# using highest p-val > 0.05 as deletion criterion

poly_mod1 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)) 
summary(poly_mod1)
plot(poly_mod1)

poly_mod2 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2)) 
summary(poly_mod2)
plot(poly_mod2)

poly_mod3 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((TOEFL - mean(TOEFL))^2)) 
summary(poly_mod3)
plot(poly_mod3)

library(EnvStats) #for boxcox
bc = boxcox(poly_mod1, lambda = seq(-2,6,0.2))
plot(bc)

which.max(bc$objective)
bc$lambda[37] # 5.2
#

poly_mod5.3 = lm(data = Admission, 
               formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)) 
summary(poly_mod5.3)
plot(poly_mod5.3)

poly_mod5.3o = lm(data = Admission[-c(outliers),], 
                 formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)) 
summary(poly_mod5.3o)
plot(poly_mod5.3o)

library(car) # for ncvTest (Breusch-pagan)
ncvTest(poly_mod5.3)
shapiro.test(poly_mod5.3$residuals)

# try adding cubic terms
cube_mod1 = lm(data = Admission, 
                 formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((GRE - mean(GRE))^3) + I((TOEFL - mean(TOEFL))^3) + I((CGPA - mean(CGPA))^3)) 
summary(cube_mod1)
plot(cube_mod1)

cube_mod2 = lm(data = Admission, 
               formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                 I((GRE - mean(GRE))^3) + I((TOEFL - mean(TOEFL))^3) + I((CGPA - mean(CGPA))^3)) 
summary(cube_mod2)
plot(cube_mod2)

cube_mod2o = lm(data = Admission[-outliers,], 
               formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                 I((GRE - mean(GRE))^3) + I((TOEFL - mean(TOEFL))^3) + I((CGPA - mean(CGPA))^3)) 
summary(cube_mod2o)
plot(cube_mod2o)

shapiro.test(cube_mod2o$residuals)
ncvTest(cube_mod2o)

## drop irrelevant terms

cube_mod2o.1 = lm(data = Admission[-outliers,], 
                formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                  I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                  I((GRE - mean(GRE))^3) + I((CGPA - mean(CGPA))^3)) 
summary(cube_mod2o.1)
plot(cube_mod2o.1)

cube_mod2o.2 = lm(data = Admission[-outliers,], 
                  formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                    I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                    I((CGPA - mean(CGPA))^3)) 
summary(cube_mod2o.2)
plot(cube_mod2o.2)

cube_mod2o.3 = lm(data = Admission[-outliers,], 
                  formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                    I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)) 
summary(cube_mod2o.3)
plot(cube_mod2o.3)

###

cube_mod2o.4 = lm(data = Admission[-outliers,], 
                  formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                  I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)) 
summary(cube_mod2o.4)
plot(cube_mod2o.4)

# I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)
poly_mod = lm(data = Admission, 
              formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2)) 

shapiro.test(poly_mod$residuals)

W = lm(abs(poly_mod$residuals) ~ poly_mod$fitted.values)
sigma_sq = W$residuals^2
weights = 1/sigma_sq

weighted_poly = lm(data = Admission, 
                   formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                     I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2), weights = weights) 
summary(weighted_poly)
plot(weighted_poly)

ncvTest(weighted_poly) 
shapiro.test(weighted_poly$residuals)
##



WO = lm(abs(cube_mod2o.4$residuals) ~ cube_mod2o.4$fitted.values)
sigma_sq_o = WO$residuals^2
weights_o = 1/sigma_sq_o



weighted_poly_O = lm(data = Admission[-outliers,], 
                   formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                     I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2), weights = weights_o) 
summary(weighted_poly_O)
plot(weighted_poly_O)
