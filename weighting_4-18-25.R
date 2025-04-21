mod_A5 = lm(data = Admission, formula = I(Y^5) ~  GRE + TOEFL + LOR + CGPA + Research)

weight_mod5 = lm(abs(mod_A5$residuals) ~ mod_A5$fitted.values)
sigma_sq = weight_mod5$residuals^2
w.i. = 1/sigma_sq

mod_A5_w = lm(data = Admission, formula = I(Y^5) ~  GRE + TOEFL + LOR + CGPA + Research, weights = w.i.)
summary(mod_A5_w)

mod_A5_out = lm(data = Admission[-c(outliers),], formula = I(Y^5) ~  GRE + TOEFL + LOR + CGPA + Research)

weight_mod5o = lm(abs(mod_A5_out$residuals) ~ mod_A5_out$fitted.values)
sigma_sq = weight_mod5o$residuals^2
w.i.2 = 1/sigma_sq

mod_A5_out_w = lm(data = Admission[-c(outliers),], formula = I(Y^5) ~  GRE + TOEFL + LOR + CGPA + Research, weights = w.i.2)


AIC(lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)) # -1059.225

AIC(lm(data = Admission, formula = Y ~   TOEFL + LOR + CGPA + Research)) # -1052.236
AIC(lm(data = Admission, formula = Y ~  GRE  + LOR + CGPA + Research))   #  -1053.082
AIC(lm(data = Admission, formula = Y ~  GRE + TOEFL  + CGPA + Research)) #  -1039.031
AIC(lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR  + Research)) # -965.6601
AIC(lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA ))  # -1051.567

mod_A4 = lm(data = Admission, formula = Y ~  GRE + LOR + CGPA + Research)
mod_A4 |> summary()
plot(mod_A4)

boxcox(mod_A4, lambda = seq(-6,8,0.5)) |> plot.boxcox()

mod_A4.5 = lm(data = Admission, formula = I(Y^5) ~  GRE + LOR + CGPA + Research)
mod_A4.5 |> summary()
plot(mod_A4.5)

mod_A4.5o = lm(data = Admission[-c(outliers),], formula = I(Y^5) ~  GRE + LOR + CGPA + Research)
plot(mod_A4.5o)

WM4.5o = lm(abs(mod_A4.5o$residuals) ~ mod_A4.5o$fitted.values)
sigma_sq = WM4.5o$residuals^2
weights = 1/sigma_sq

mod_A4.5o.w = lm(data = Admission[-c(outliers),], formula = I(Y^5) ~  GRE + LOR + CGPA + Research,weights = weights)

plot(lm(data = Admission, formula = Y ~  GRE))
plot(lm(data = Admission, formula = Y ~  TOEFL))
plot(lm(data = Admission, formula = Y ~  LOR))
plot(lm(data = Admission, formula = Y ~  CGPA))
plot(lm(data = Admission, formula = Y ~  Research))

#### 

mod_gpa = lm(data = Admission, formula = Y ~  CGPA)
boxcox(mod_gpa, lambda = seq(-6,8,0.5)) |> plot.boxcox()

mod_gpa5 = lm(data = Admission, formula = I(Y^5) ~  CGPA)
summary(mod_gpa5)
plot(mod_gpa5)

WM4.5gpa = lm(abs(mod_gpa5$residuals) ~ mod_gpa5$fitted.values)
sigma_sq = WM4.5gpa$residuals^2
weights = 1/sigma_sq

mod_gpa5w = lm(data = Admission, formula = I(Y^5) ~  CGPA, weights = weights)
plot(mod_gpa5w)



mod_gpa_o = lm(data = Admission[-c(outliers),], formula = Y ~  CGPA)
summary(mod_gpa_o)
plot(mod_gpa_o)


mod_gpa_o5 = lm(data = Admission[-c(outliers,291,91,299,92),], formula = I(Y^5) ~  CGPA)
summary(mod_gpa_o5)
plot(mod_gpa_o5)

WM4.5gpa_o = lm(abs(mod_gpa_o5$residuals) ~ mod_gpa_o5$fitted.values)
sigma_sq = WM4.5gpa_o$residuals^2
weights = 1/sigma_sq

mod_gpa_o5w = lm(data = Admission[-c(outliers,291,91),], formula = I(Y^5) ~  CGPA, weights = weights)
summary(mod_gpa_o5w)
plot(mod_gpa_o5w)

mod_gpa_sq = lm(data = Admission, formula = Y ~  CGPA + I(CGPA^2))
