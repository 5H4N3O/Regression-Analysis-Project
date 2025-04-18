library(ExhaustiveSearch)

es_AIC = ExhaustiveSearch(formula = Y ~ ., data = data, family = 'gaussian', performanceMeasure = "AIC")
print(es_AIC)

es_MSE = ExhaustiveSearch(formula = Y ~ ., data = data, family = 'gaussian', performanceMeasure = "MSE")
print(es_MSE)


data <- read.table("C:\\Users\\rehbe\\UMD_Undergrad\\Spring_2025\\Stat4511_RegAn\\Project\\archive\\Admission_Predict.csv", header = TRUE, sep = ",")
colnames(data) <-c("No.","GRE","TOEFL","UniRating","SOP","LOR" ,"CGPA","Research","Y")
data = data[,2:9] # To remove No.

# Best model by AIC:  GRE + TOEFL + LOR + CGPA + Research
# narrowly beats: GRE + TOEFL + UniRating + LOR + CGPA + Research

library(leaps)
back_model = regsubsets(x = Y ~ GRE + TOEFL + UniRating + SOP + LOR + CGPA + Research, data = data, method = c("backward"))
summary(back_model)

for_model = regsubsets(x = Y ~ ., data = data, method = c("forward"))
summary(for_model)

sw_model = regsubsets(x = Y ~ ., data = data, method = c("seqrep"))
summary(sw_model)
  
###
# Compare  GRE + TOEFL + LOR + CGPA + Research
# versus
#          GRE + TOEFL + UniRating + LOR + CGPA + Research

model_5var = lm(data = data, formula = Y ~ GRE + TOEFL + LOR + CGPA + Research)
model_6var = lm(data = data, formula = Y ~ GRE + TOEFL + UniRating + LOR + CGPA + Research)


# adj r2 5 var: 0.8002 
# adj r2 6 var  0.8003 

library(lme4)

BIC(model_5var) # -1031.284
BIC(model_6var) # -1026.454
