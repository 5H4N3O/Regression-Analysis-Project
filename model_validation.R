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

library(car) # for ncvTest (Breusch-pagan)

ncvTest(model_A2) # 0.0011912, heteroskedasticity present

### Do WLS regression and see how that works out

weight_mod = lm(abs(model_A2$residuals) ~ model_A2$fitted.values)
sigma_sq = weight_mod$residuals^2
w.i. = 1/sigma_sq

model_weighted = lm(data = Admission, formula = I(Y^2) ~  GRE + TOEFL + LOR + CGPA + Research, weights = w.i.)
model_weighted |> summary()
plot(model_weighted)

shapiro.test(model_weighted$residuals) # still nonormal 



