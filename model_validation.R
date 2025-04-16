# using model A, see if it violates assumptions of regression

model_A = lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)

# There is linear relation w/ P <  2.2e-16

qqnorm(model_A$residuals)
shapiro.test(model_A$residuals) # 1.443e-13
# non-normal residuals
plot(model_A) # residuals vs. leverage looks fine

# residuals not normal, use box=cox transformation
library(EnvStats) #for boxcox
bc = boxcox(model_A) # use y^2 transformation

which.max(bc$y)
bc$x[100]

model_A2 = lm(data = Admission, formula = I(Y^2) ~  GRE + TOEFL + LOR + CGPA + Research)
plot(model_A2)

boxcox(model_A2)
shapiro.test(model_A2$residuals) # P small

# Cannot fix normality assumption, apply other remedials to model and use 
# bootstrap PIs and CIs 