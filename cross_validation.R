# Using LOOCV to again check if 5 or 6 variable model is better

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

# Shows that 6 variable model is not significantly better predictor than 5 variable