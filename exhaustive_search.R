library(ExhaustiveSearch)

es_AIC = ExhaustiveSearch(formula = Y ~ ., data = data, family = 'gaussian', performanceMeasure = "AIC")
print(es_AIC)


data <- read.table("C:\\Users\\rehbe\\UMD_Undergrad\\Spring_2025\\Stat4511_RegAn\\Project\\archive\\Admission_Predict.csv", header = TRUE, sep = ",")
colnames(data) <-c("No.","GRE","TOEFL","UniRating","SOP","LOR" ,"CGPA","Research","Y")
data = data[,2:9] # To remove No.




