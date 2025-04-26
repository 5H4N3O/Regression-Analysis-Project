# Importing data
Admission <- read.table("./Admission_Predict.csv", header = TRUE, sep = ",")

Admission = Admission[,which(names(Admission) != "Serial.No.")] # remove serial.no.
library(dplyr) # for rename
Admission = rename(Admission, 
                   c(GRE= "GRE.Score", TOEFL = "TOEFL.Score", UniRating = "University.Rating", Y = "Chance.of.Admit"))  
Admission |> head()

library(corrplot) # for corrplot
library(regclass) # for VIF

M = cor(Admission)
corrplot(M, method = 'square', order = 'alphabet', type = 'lower')

full_model = lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research + UniRating + SOP)
VIF(full_model) |> round(3)

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
# looks like we 'overshot' with Y^6 instead of Y^5
# use 5th power transform instead, 

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

# CIs

# since the family wise confidence level will be 90%, each CI will use 
# alpha = 0.1/9 = 0.0111... , i.e. each CI is at 98.889% level

set.seed(123)

sample_from = setdiff(1:400, c( 66,  67  ,69, 116, 360)) 
# only want to sample from the data used to construct model

trials = 1000
coeff_values = data.frame(matrix(nrow = 1000, ncol = 9))
colnames(coeff_values) = c("b0","b1","b2","b3","b4","b5","b22","b44","b444")

for(i in 1:trials){
  # Generate sample
  indices = sample(sample_from, size = 395,replace = T) 
  bt_samp = Admission[indices,]  
  
  # First, fit unweighted model
  mod1 = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
              I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3),
            data = bt_samp)
  
  # Use that to generate weights
  res = mod1$residuals
  fitted = mod1$fitted.values
  mod = lm(formula = abs(res) ~ fitted)
  
  var = mod$fitted.values^2
  sim_weights = 1/var
  
  # Get coefficient estimates of weighted model
  sim_mod = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3),
               data = bt_samp, weights = sim_weights)
  
  # Record these coefficients
  coeff_values[i,] = coefficients(sim_mod)
  # and repeat :)
}

write.csv(coeff_values,"./bootstrap_coefficients",row.names = F)

library(ggplot2)
# Generate histograms for each coefficient

plot_b0 = ggplot(data = coeff_values, aes(x = b0)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b0 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot_b1 = ggplot(data = coeff_values, aes(x = b1)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b1 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


plot_b2 = ggplot(data = coeff_values, aes(x = b2)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b2 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


plot_b3 = ggplot(data = coeff_values, aes(x = b3)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b3 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


plot_b4 = ggplot(data = coeff_values, aes(x = b4)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b4 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


plot_b5 = ggplot(data = coeff_values, aes(x = b5)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b5 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


plot_b22 = ggplot(data = coeff_values, aes(x = b22)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b22 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot_b44 = ggplot(data = coeff_values, aes(x = b44)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b44 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot_b444 = ggplot(data = coeff_values, aes(x = b444)) +
  theme_classic() +
  geom_histogram(bins = 50) + 
  ylab("") +
  labs(title = "Simulated b444 values") +
  xlab("") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


library(ggpubr) # for ggarrange

ggarrange(plot_b0,plot_b1,plot_b2,plot_b3,plot_b4,plot_b5,plot_b22,plot_b44,plot_b444)

# Get quanitle values
family_alpha = 0.1
alpha = family_alpha/9

quantile(coeff_values["b0"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b1"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b2"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b3"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b4"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b5"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b22"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b44"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b444"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)

printout = function(df){
  df = unname(df)
  s = c("$","(", df[1], "\\hspace{.1cm}, \\hspace{.1cm}",df[2], ")","$")
  s = cat(s)
  return(paste(s, collapse = ""))
}

# Using final model and constructing a couple PIs 

h = hatvalues(cubic.wls)
min(h) # 0.006827814
max(h) # 0.4296308
# ^ range of acceptable values for making predictions and ensure no extrapolation

# See how presence of research affects admission chance at different levels of other variables
# Average Grades/test scores
to_predict1.1 = data.frame("GRE" = 320, "TOEFL" = 110, "LOR" = 4.0, "CGPA" = 8.6, "Research" = 0)
to_predict1.2 = data.frame("GRE" = 320, "TOEFL" = 110, "LOR" = 4.0, "CGPA" = 8.6, "Research" = 1)
# Poor Grades/test scores
to_predict2.1 = data.frame("GRE" = 310, "TOEFL" = 105, "LOR" = 3.0, "CGPA" = 7.5, "Research" = 0)
to_predict2.2 = data.frame("GRE" = 310, "TOEFL" = 105, "LOR" = 3.0, "CGPA" = 7.5, "Research" = 1)
# Excellent Grades/test scores
to_predict3.1 = data.frame("GRE" = 330, "TOEFL" = 113, "LOR" = 5.0, "CGPA" = 9.8, "Research" = 0)
to_predict3.2 = data.frame("GRE" = 330, "TOEFL" = 113, "LOR" = 5.0, "CGPA" = 9.8, "Research" = 1)

# Check to make sure any predictions aren't extrapolation

X = model.matrix(cubic.wls) |> unname()

obs1 = matrix(data = c(1,320, 110, 4.0,8.61, 0, (110 - 107.3823)^2, (8.6-8.594759)^2,  (8.6-8.594759)^3), 
              ncol = 1)
obs2 = matrix(data = c(1,320, 110, 4.0,8.61, 1, (110 - 107.3823)^2, (8.6-8.594759)^2,  (8.6-8.594759)^3), 
              ncol = 1)
obs3 = matrix(data = c(1,310, 105, 3.0,7.5, 0, (105 - 107.3823)^2, (7.5-8.594759)^2,  (7.5-8.594759)^3), 
              ncol = 1)
obs4 = matrix(data = c(1,310, 105, 3.0,7.5, 1, (105 - 107.3823)^2, (7.5-8.594759)^2,  (7.5-8.594759)^3), 
              ncol = 1)
obs5 = matrix(data = c(1,330, 113, 5.0,9.8, 0, (113 - 107.3823)^2, (9.8-8.594759)^2,  (9.8-8.594759)^3), 
              ncol = 1)
obs6 = matrix(data = c(1,330, 113, 5.0,9.8, 1, (113 - 107.3823)^2, (9.8-8.594759)^2,  (9.8-8.594759)^3), 
              ncol = 1)

t(obs1) %*% (t(X) %*% X)^-1 %*% obs1  # 0.07276908
t(obs2) %*% (t(X) %*% X)^-1 %*% obs2  # 0.1252422
t(obs3) %*% (t(X) %*% X)^-1 %*% obs3  # -1.318934
t(obs4) %*% (t(X) %*% X)^-1 %*% obs4  # -1.300921
t(obs5) %*% (t(X) %*% X)^-1 %*% obs5  # 3.418855
t(obs6) %*% (t(X) %*% X)^-1 %*% obs6  # 3.596004

# only first 2 points not extrapolation, just look at those

to_predict1.1 = data.frame("GRE" = 320, "TOEFL" = 110, "LOR" = 4.0, "CGPA" = 8.6, "Research" = 0)
to_predict1.2 = data.frame("GRE" = 320, "TOEFL" = 110, "LOR" = 4.0, "CGPA" = 8.6, "Research" = 1)


set.seed(777) # for reproducibility

trials = 5000

predicted_values = data.frame(matrix(nrow = 5000, ncol = 2))
colnames(predicted_values) = c("Research", "No Research")

for(i in 1:trials){
  # Generate sample
  indices = sample(sample_from, size = 395,replace = T) 
  bt_samp = Admission[indices,]  
  
  # First, fit unweighted model
  mod1 = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
              I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3),
            data = bt_samp)
  
  # Use that to generate weights
  res = mod1$residuals
  fitted = mod1$fitted.values
  mod = lm(formula = abs(res) ~ fitted)
  
  var = mod$fitted.values^2
  sim_weights = 1/var
  
  sim_mod = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3),
               data = bt_samp, weights = sim_weights)
  
  # make predictions with simulated model & record them
  predicted_values[i,1] = predict(sim_mod, to_predict1.2) # with research
  predicted_values[i,2] = predict(sim_mod, to_predict1.1) # w/o research  
  # repeat :)
}

write.csv(predicted_values, "./bt_prediction", row.names = F)


library(ggplot2)

# Generate histograms for each prediction
plot_no_research = ggplot(data = predicted_values, aes(x = `No Research`^(1/5))) +
  theme_classic() +
  geom_histogram(bins = 100) + 
  ylab("Frequency") +
  labs(title = "Predictions for no undergrad research") +
  xlab("Reverse-transformed probability") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot_research = ggplot(data = predicted_values, aes(x =  Research^(1/5))) +
  theme_classic() +
  geom_histogram(bins = 100) + 
  ylab("Frequency") +
  labs(title = "Predictions for completed undergrad research") +
  xlab("Reverse-transformed probability") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))



# Want the 2 PI's at 95% level, family wide
# PIs are calculated on transformed data and endpoints are then translated back into
# original terms
family_alpha = 0.05
alpha = family_alpha/2 #0.025

quantile(predicted_values$Research, probs = c(alpha/2, 1 - alpha / 2)) 
# 0.2471215 0.2819776 
quantile(predicted_values$`No Research`, probs = c(alpha/2, 1 - alpha / 2)) 
# 0.2128977 0.2486968 

# in terms of original data
(c(0.2471215, 0.2819776 )^(1/5)) |> round(4) #  0.7561 0.7763
(c(0.2128977, 0.2486968  )^(1/5)) |> round(4) #  0.7339 0.7571

ggarrange(plot_research, plot_no_research, nrow = 2)

## Plot of both at once

double_plot = ggplot(data = predicted_values, aes(x = value, fill = key)) +
  theme_classic() +
 # geom_histogram(aes(x = Research^(1/5)), fill = "red", position="identity", alpha=0.5, bins = 100) +
  #geom_histogram(aes(x = `No Research`^(1/5)), fill = "blue", position="identity", alpha=0.5, bins = 100) 
  geom_density(aes(x = Research^(1/5)), fill = "red", position="identity", alpha=0.5) +
  geom_density(aes(x = `No Research`^(1/5)), fill = "blue", position="identity", alpha=0.5) +
  scale_y_continuous(expand = c(0,0), labels = c()) +
  scale_x_continuous(limits = c(0.728, 0.7825),breaks = c(0.73,0.74,0.75,.76,0.77,.78)) +
  labs(title = "Predicted Acceptance Probabilities",
       y = "Density",
       x = "Probability of Acceptance") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
double_plot

library(tidyr) # for gather

double_plot2 = ggplot(data = gather(predicted_values^(1/5)), aes(x = value, fill = key)) +
  theme_classic() + 
  geom_density(alpha = 0.5) + 
  scale_y_continuous(expand = c(0,0), labels = c()) +
  scale_x_continuous(limits = c(0.728, 0.7825),breaks = c(0.73,0.74,0.75,.76,0.77,.78)) +
  labs(title = "Predicted Acceptance Probabilities",
       y = "Density",
       x = "Probability of Acceptance",
       fill = "") +
  scale_fill_manual(values = c("#20DFAB","#DF2054")) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),     
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size=13),
        legend.text = element_text(size=13),
        legend.position = c(0.9,0.92))
double_plot2
