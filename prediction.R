# Using final model and constructing a couple PIs 

sample_from = setdiff(1:400, c( 66,  67  ,69, 116, 360)) 

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

preds = list(to_predict1.1,to_predict1.2,to_predict2.1,to_predict2.2,to_predict3.1,to_predict3.2)

# Check to make sure any predictions aren't extrapolation

X = model.matrix(cubic.wls) |> unname()

mean(Admission[sample_from,]$TOEFL) # 107.3823
mean(Admission[sample_from,]$CGPA) # 8.594759


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
