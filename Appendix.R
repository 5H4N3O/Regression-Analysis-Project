# Importing data
Admission <- read.table("./Admission_Predict.csv", header = TRUE, sep = ",")

Admission = Admission[,which(names(Admission) != "Serial.No.")] # remove serial.no.
library(dplyr) # for rename
Admission = rename(Admission, 
                   c(GRE= "GRE.Score", TOEFL = "TOEFL.Score", UniRating = "University.Rating", Y = "Chance.of.Admit"))  
Admission |> head()


# Model Selection

library(ExhaustiveSearch)

# Exhaustive search via AIC
es_AIC = ExhaustiveSearch(formula = Y ~ ., data = Admission, family = 'gaussian', performanceMeasure = "AIC")
print(es_AIC)
# Top models are: 
  # GRE + TOEFL + LOR + CGPA + Research
    # AIC: -1059.225                     
  # GRE + TOEFL + UniRating + LOR + CGPA + Research
    # AIC: -1058.386  

model_5var = lm(data = Admission, formula = Y ~ GRE + TOEFL + LOR + CGPA + Research)
model_6var = lm(data = Admission, formula = Y ~ GRE + TOEFL + UniRating + LOR + CGPA + Research)
summary(model_5var)
summary(model_6var)
# Adj R^2 of 5 var : 0.8002
# Adj R^2 of 6 var : 0.8003 

library(lme4) # for BIC

BIC(model_5var) # -1031.284
BIC(model_6var) # -1026.454

library(qpcR) # for PRESS

PRESS(model_5var, verbose = FALSE) # 0.796567
PRESS(model_6var, verbose = FALSE) # 0.7962553

# PRESS for 6 var model is slightly higher than PRESS of 5 var model 

library(olsrr) # for mallow's CP

full_model = lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research + UniRating + SOP)

ols_mallows_cp(model_5var, full_model) # 5.494153
ols_mallows_cp(model_6var, full_model) # 6.353168

# 6 variable model slightly less biased

# So: 
# AIC and BIC favor 5 variable model
# Adj R2, PRESS, and CP favor 6 variable model
# Cross validation necessary


# Cross Validation

# CV will be conducted using leave-one-out method

TotalPE_5 = 0
TotalPE_6 = 0 

N = 400

for(i in 1:N){
  mod_5 = lm(data = Admission[-c(i),], formula = Y ~GRE + TOEFL + LOR + CGPA + Research)
  mod_6 = lm(data = Admission[-c(i),], formula = Y ~GRE + TOEFL + LOR + CGPA + Research + UniRating)
  
  PE_5 = Admission[i,"Y"] - predict(mod_5,  Admission[i,])
  PE_6 = Admission[i,"Y"] - predict(mod_6,  Admission[i,])
  
  TotalPE_5 = TotalPE_5 + PE_5^2
  TotalPE_6 = TotalPE_6 + PE_6^2  
}

MSPE_5 = TotalPE_5/N
MSPE_6 = TotalPE_6/N

round(MSPE_5, 5) # 0.00413  
round(MSPE_6, 5) # 0.00413   

# So, the 5 and 6 variable models have almost the exact same predictive power
# Given this and given that the extra variable (UniRating) in 6 variable model  
  # is not statistically significant (P-value = 0.29), 5 variable model should be selected
  # for simplicity
  

# Fixing Model Assumptions

library(car) # for ncvTest (Breusch-pagan)

base_model = lm(data = Admission, formula = Y ~ GRE + TOEFL + LOR + CGPA + Research)
plot(base_model) 
# residuals look nonnormal and heteroskedastic, no large residuals via Cook's Distance however

shapiro.test(base_model$residuals) # not normal
ncvTest(base_model) # heteroskedasticity present


# Plots on predictors vs Y to detect violations of linearity between Y and each predictor
pairs(Admission[c("Y","GRE", "TOEFL", "UniRating", "LOR", "CGPA","Research")], lower.panel = NULL)
# Y appears to be linear wrt. each predictor, no violation there
 # multicolinearity present, but the desire is to use model to make predictions in scope
 # of data, nothing will be done about it

# So, linearity and outlier assumptions seem fine, need to fix error distrubtion
# Done using Boxcox transformation

library(EnvStats) # for Boxcox

bc = boxcox(base_model, lambda = seq(-6,10, 0.1))
plot(bc) # optimal value appears to be around 5 or 6
bc$lambda[which.max(bc$objective)] # 5.6
# Use Y^6 transform model

model_Y6 = lm(data = Admission, formula = I(Y^6) ~ GRE + TOEFL + LOR + CGPA + Research)
plot(model_Y6)
shapiro.test(model_Y6$residuals) # not normal
ncvTest(model_Y6) # heteroskedasticity present

# Normality did get better, but heteroskedasticity is still bad, introduce polynomial
 # terms to model to try to fix this

res = model_A$residuals

plot(Admission$GRE ,res)
plot(Admission$TOEFL ,res)
plot(Admission$LOR ,res)
plot(Admission$CGPA ,res)
plot(Admission$Research ,res)

# residual plots show that there is a 'dipping' effect in each continious variable's
 # plot, indicating want for higher-order terms

# Polynomial Model Fitting

# A hierarchical approach is taken, wherein a polynomial model featuring quadratic and
 # cubic terms for GRE, TOEFL, and CGPA are introduced. These terms are iteratively deleted
 # by taking the one with the highest p-value > 0.05 and removing it. If a lower-order term
 # is removed this way, the higher power terms will also be removed. This will be repeated until 
 # all terms are relevant.
 # Additionally, variables are centered for higher-order terms

poly_mod1 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                 I((GRE - mean(GRE))^3) + I((TOEFL - mean(TOEFL))^3) + I((CGPA - mean(CGPA))^3))
summary(poly_mod1)
# Least relevant term is (centered) TOEFL ^ 2, remove that and it's cubic term

poly_mod2 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2)  + I((CGPA - mean(CGPA))^2) +
                 I((GRE - mean(GRE))^3)  + I((CGPA - mean(CGPA))^3))
summary(poly_mod2)
# Least relevant term is (centered) CGPA ^ 2, remove that and it's cubic term

poly_mod3 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((GRE - mean(GRE))^3))
summary(poly_mod3)
# Cubic term not relevant, remove that

poly_mod4 = lm(data = Admission, 
               formula = Y ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2))
summary(poly_mod4)
# And last polynomial term is irrelevant
 # i.e. we've collapsed back into original model


# This same approach is taken except starting with Y^6 instead of Y
poly_mod2.1 = lm(data = Admission, 
               formula = I(Y^6) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                 I((GRE - mean(GRE))^3) + I((TOEFL - mean(TOEFL))^3) + I((CGPA - mean(CGPA))^3))
summary(poly_mod2.1)
# Least relevant is (centered) TOEFL^3, remove that

poly_mod2.2 = lm(data = Admission, 
                 formula = I(Y^6) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((GRE - mean(GRE))^3)  + I((CGPA - mean(CGPA))^3))
summary(poly_mod2.2)
# Least relevant is GRE^3, remove that

poly_mod2.3 = lm(data = Admission, 
                 formula = I(Y^6) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((CGPA - mean(CGPA))^3))
summary(poly_mod2.3)
# Least relevant is GRE^2, remove that

poly_mod2.4 = lm(data = Admission, 
                 formula = I(Y^6) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((CGPA - mean(CGPA))^3))
summary(poly_mod2.4)
# All terms relevant; stop here

cubic_model = lm(data = Admission, 
                 formula = I(Y^6) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3))
plot(cubic_model)

ncvTest(cubic_model)
shapiro.test(cubic_model$residuals)

boxcox(cubic_model, lambda = seq(-6,6,0.1)) |> plot()
bc2 = boxcox(cubic_model, lambda = seq(-3,3,0.01))
bc2$lambda[which.max(bc2$objective)] # 0.89
# .89 * 6 = 5.34
# use 5th power transform instead

# Redo hierarchical approach, starting at Y^5 instead of Y^6 

poly_mod3.1 = lm(data = Admission, 
                 formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((GRE - mean(GRE))^3) + I((TOEFL - mean(TOEFL))^3) + I((CGPA - mean(CGPA))^3))
summary(poly_mod3.1) 
# Drop TOEFL^3
poly_mod3.2 = lm(data = Admission, 
                 formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((GRE - mean(GRE))^3) +  I((CGPA - mean(CGPA))^3))
summary(poly_mod3.2)
# drop GRE^3
poly_mod3.3 = lm(data = Admission, 
                 formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((GRE - mean(GRE))^2) + I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((CGPA - mean(CGPA))^3))
summary(poly_mod3.3)
# Drop GRE^2
poly_mod3.4 = lm(data = Admission, 
                 formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                   I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) +
                   I((CGPA - mean(CGPA))^3))
summary(poly_mod3.4)

shapiro.test(poly_mod3.4$residuals)
ncvTest(poly_mod3.4)
# Even still, nonnormal, heteroskaditic errors

# Further investiagtion done into outliers to see if omitting strong outliers 
  # can significantly improve model fit
# As plot show, points which weren't notable outliers in the original model may become
 # strong outliers in new model


p = 8
n = 400

# Outlying X obser.

H = hatvalues(poly_mod3.4)
(H > (2 * p / n)) |> which() |> unique() |> sort()
H |> sort(decreasing = TRUE) |> head(20)
#25  29  30  35  39  48  51  53  57  59  72  79  80  98 118 119 131 144 149 169 177 203 204 214 252 258
#27 285 298 345 346 348 349 369 385 386 
# ^ strongly influential X
# 59 is by far strongest point here, 4x bigger h value than next highest

dff = dffits(poly_mod3.4)
(dff > (2*sqrt(p / n))) |> which() |> unique() |> sort()
# 83 287 359 360

dff[c(83, 287, 359, 360)]
# 360 is very influential to fitted values

cd = cooks.distance(poly_mod3.4)
(qf(cd, p, n-p) > 0.5) |> which() # none :)

dfb = dfbeta(poly_mod3.4)
(dfb > (2/sqrt(n))) |> which() # none here :)

# Per the plots, it looks like 66,67,69 are things throwing off residual vs. fitted and normal
  # qq plot, look at Y outliers instead

library(MASS) # for studentized residuals

sr = studres(poly_mod3.4)
rejection = qt(1 - 0.05/(2*n), n - p - 1)
(abs(sr) > rejection) |> which() |> unique()
# 69 is outlier wrt. y

(abs(sr) > 3) |> which() |> unique() # using slightly lower rejection criteria
# 66  67  69 116 360
# try omitting these 5 points

cubic_mod_omitOutliers = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                           I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3), 
                                  data = Admission[-c( 66,  67  ,69, 116, 360), ])

shapiro.test(cubic_mod_omitOutliers$residuals) # 0.09864
ncvTest(cubic_mod_omitOutliers) # 0.00068006
# Heteroskedasticity still present, but residuals are now reasonably normal
 # weighting can now be applied to model


# Model Weighting

res = cubic_mod_omitOutliers$residuals
fitted = cubic_mod_omitOutliers$fitted.values
mod = lm(formula = abs(res) ~ fitted)

var = mod$fitted.values^2
w.i = 1/var

cubic.wls = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3),
               data = Admission[-c( 66,  67  ,69, 116, 360), ],
               weights = w.i)
summary(cubic.wls)
plot(cubic.wls)

shapiro.test(cubic.wls$residuals) # 0.2
ncvTest(cubic.wls) # 0.41676
# So, this weighted cubic model does meet assumptions of regression :)


