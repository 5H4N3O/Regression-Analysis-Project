# using model A, see if it violates assumptions of regression

model_A = lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)

# There is linear relation w/ P <  2.2e-16

qqnorm(model_A$residuals)
shapiro.test(model_A$residuals) # 1.443e-13
# non-normal residuals
plot(model_A) # residuals vs. leverage looks fine


