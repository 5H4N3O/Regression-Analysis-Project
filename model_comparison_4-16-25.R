GPA <- read.table("./Admission_Predict.csv", header = TRUE, sep = ",")

GPA = GPA[,which(names(GPA) != "Serial.No.")] # remove serial.no.
library(dplyr) # for rename
GPA = rename(GPA, c(GRE= "GRE.Score", TOEFL = "TOEFL.Score", UniRating = "University.Rating", Y = "Chance.of.Admit"))  
GPA |> head()


#write.csv(GPA, "./GPA.csv",row.names = FALSE)

# Comparing the best 2 models comparing to exhaustive AIC search
model_A = lm(data = GPA, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research)
model_B = lm(data = GPA, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research + UniRating )
full_model = lm(data = GPA, formula = Y ~  GRE + TOEFL + LOR + CGPA + Research + UniRating + SOP)

library(lme4) # for BIC

library(regclass) # for VIF
# want to check if anything in model B exceeds VIF > 5 that doesn't in model A, 
# to quickly rule out model B
VIF(model_B)
VIF(model_A)
# no
# VIF of GRE and TOEFL and CGPA > 4, investigate later

BIC(model_A) #-1031.284
BIC(model_B) #-1026.454
# BIC of A < BIC of B -> model A better


model_A |> summary()
model_B |> summary()

# Adj R^2 of A = 0.8002 
# Adj R^2 of B = 0.8003 

library(qpcR) # for PRESS

PRESS(model_A, verbose = FALSE) # 0.796567
PRESS(model_B, verbose = FALSE) # 0.7962553

# PRESS B slightly less than PRESS A

library(olsrr) # for mallow's CP

ols_mallows_cp(model_A, full_model) # 5.494153
ols_mallows_cp(model_B, full_model) # 6.353168

# predictors in model A = 5
# predictors in model B = 6

abs(5 - ols_mallows_cp(model_A, full_model)) # 0.4941531
abs(6 - ols_mallows_cp(model_B, full_model)) # 0.3531678

# Cp B is slightly closer to p than Cp A is

# With all of all of this, there is still the fact that t-val for 
# uni rating is still > 0.05 (=0.29)
# Given that and given that AIC BIC do still slightly favor A, pick A as final model?


