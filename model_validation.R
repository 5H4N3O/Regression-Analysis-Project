# using model A, see if it violates assumptions of regression

model_A = lm(data = Admission, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)

# There is linear relation w/ P <  2.2e-16

qqnorm(model_A$residuals)
shapiro.test(model_A$residuals) # 1.443e-13
# non-normal residuals
plot(model_A) # residuals vs. leverage looks fine

# residuals not normal, use box=cox transformation
library(EnvStats) #for boxcox
bc = boxcox(model_A) # use y^2 transformation?
bc2 = boxcox(model_A, lambda = seq(-5, 5, by = 0.1))
plot.boxcox(bc)
plot.boxcox(boxcox(model_A, lambda = seq(-8, 8, by = 0.1)))

mod_A5 = lm(data = Admission, formula = I(Y^5) ~  GRE + TOEFL + LOR + CGPA + Research)

#which.max(bc$y)
#bc$x[100]

model_A2 = lm(data = Admission, formula = I(Y^2) ~  GRE + TOEFL + LOR + CGPA + Research)
plot(model_A2)

boxcox(model_A2)
shapiro.test(model_A2$residuals) # P small

# Cannot fix normality assumption, apply other remedials to model and use 
# bootstrap PIs and CIs 

library(car) # for ncvTest (Breusch-pagan)

ncvTest(model_A2) # 0.0011912, heteroskedasticity present
ncvTest(model_A) # 1.4473e-10


### Do WLS regression and see how that works out

weight_mod = lm(abs(model_A2$residuals) ~ model_A2$fitted.values)
sigma_sq = weight_mod$residuals^2
w.i. = 1/sigma_sq

model_weighted = lm(data = Admission, formula = I(Y^2) ~  GRE + TOEFL + LOR + CGPA + Research, weights = w.i.)
model_weighted |> summary()
plot(model_weighted)

shapiro.test(model_weighted$residuals) # still nonormal 

# perhaps outliers in original dataset are to blame

# finding X outliers
hat_A = hatvalues(model_A) 
hat_A[hat_A > 0.05] # observations 53 and 59 are X outliers
# 2p / n = 2 * 5 / 400 = 0.025

mod_A_dffits= dffits(model_A)
p =5
n= 400
2 * sqrt(p/n) #  0.2236068
sort(mod_A_dffits, decreasing  = TRUE) |> head(10)
#  360       359       328        57        53       256       349    
#  ^ influential cases
# via plot(model_A), we see no points influential based on Cook's distance

betas = dfbetas(model_A)
# 2/sqrt(400) = 0.1
betas > 0.1
list = c()
for(i in 2:6){
  list = c(list,((betas > 0.1)[,i] |> which())) 
}

unique(list) |> sort()
# 8  10  11  19  30  40  41  42  43  51  53  56  57  59  60  61  63  65  66  67  69  76  81  93  94 104 113 116 117
# 30 120 125 150 169 179 183 256 264 270 274 328 330 332 333 358 359 360 361 368 370 375 376

# ^ influence high on at least on beta

# 53 appears in all of the precedding metrics

model_A$residuals |> mean() # -5.802667e-18
model_A$residuals[53] # 0.07062589
which.max(model_A$residuals)
model_A$residuals[328] # 0.1598973 


Admission_cut = Admission[-c(53,328),]
model_A_omit = lm(data = Admission_cut, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)

# omitting a couple outliers doesnt particularly change model fit, not super suprising

# try cubic transform instead

model_A3 = lm(data = Admission, formula = I(Y^3) ~  GRE + TOEFL + LOR + CGPA + Research)
plot(model_A3)
summary(model_A3)
shapiro.test(model_A3$residuals) # :(


# instead of first looking into normality, first investigate linearity assumption of 
# predictor variables

pairs(Admission[c("Y","GRE", "TOEFL", "UniRating", "LOR", "CGPA","Research")], lower.panel = NULL)

# relationships look linear between Y and other variables, however, a lot of 
# multicolinearity seems present

library(regclass) # for VIF
VIF(model_A) # GRE, TOEFL, and CGPA have concerning VIF, introduce interaction terms involved w/
              # those first

model_interactions = lm(data = Admission, formula = 
          Y ~ GRE + TOEFL + LOR + CGPA + Research +
            I(GRE * TOEFL) + I(GRE * LOR) + I(GRE * CGPA) + I(GRE * Research) +
            I(TOEFL *LOR) + I(TOEFL * CGPA) + I(TOEFL * Research) +
            I(CGPA * LOR) + I(CGPA * Research))
plot(model_interactions)
summary(model_interactions)

# perhaps only few interaction terms required

model_interactions2 = lm(data = Admission, formula = 
                          Y ~ GRE + TOEFL + LOR + CGPA + Research +
                          I(GRE * TOEFL) + I(GRE * LOR) + I(GRE * CGPA) + I(GRE * Research))
plot(model_interactions2)
summary(model_interactions2)

# use ridge regression instead? 

library(glmnet)

x_vars = data.matrix(Admission[,c("GRE", "TOEFL", "UniRating", "LOR", "CGPA","Research")])
y_var = Admission$Y
lambda_seq = 10^seq(2, -2, by = -.1)
fit = glmnet(x_vars, y_var, alpha =0, lambda= lambda_seq)
summary(fit)

# Using cross validation glmnet
ridge_cv <- cv.glmnet(x_vars, y_var, alpha = 0)
best_lambda = ridge_cv$lambda.min
best_fit = ridge_cv$glmnet.fit
best_ridge = glmnet(x_vars, y_var, alpha = 0, lambda = best_lambda)
coef(best_ridge)


# remove more outliers?
dfbeta_outliers = unique(list)
Admission_cut2 = Admission[-c(dfbeta_outliers),]

model_A_omit3 = lm(data = Admission_cut2, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)
bc_o = boxcox(model_A_omit3)
plot(bc_o)
boxcox(model_A_omit3, optimize = TRUE) # still use lambda = 2

model_AO2 = lm(data = Admission_cut2, formula = I(Y^2) ~  GRE + TOEFL + LOR + CGPA + Research)
summary(model_AO2)
plot(model_AO2)
shapiro.test(model_AO2$residuals)

library(car) # for ncvTest (Breusch-pagan)
ncvTest(model_AO2) # 0.0015866

# also remove outliers as determined by dffits?
outliers = c(dfbeta_outliers, 360, 359, 328, 57, 53 ,256,349   )
outliers = unique(outliers) |> sort()
Admission_cut3 = Admission[-c(outliers),]
pairs(Admission_cut3[c("Y","GRE", "TOEFL", "UniRating", "LOR", "CGPA","Research")], lower.panel = NULL)

model_A_o4 = lm(data = Admission_cut3, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)
plot(model_A_o4)
boxcox(model_A_o4, optimize = TRUE)
model_A2_o4 = lm(data = Admission_cut3, formula = I(Y^2) ~  GRE + TOEFL + LOR + CGPA + Research)
model_A2_o4 |> summary()
plot(model_A2_o4)
