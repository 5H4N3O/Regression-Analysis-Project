data <- read.table("./Admission_Predict.csv", header = TRUE, sep = ",")
colnames(data) <-c("No.","GRE","TOEFL","UniRating","SOP","LOR" ,"CGPA","Research","Y")
data <- as.data.frame(data)

plot(data$GRE, data$Y)

model <- lm(formula = Y ~ GRE + TOEFL + UniRating + SOP + LOR + CGPA + Research, data = data)
summary(model)
plot(model)