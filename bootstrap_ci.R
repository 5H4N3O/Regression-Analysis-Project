cubic.wls = lm(formula = I(Y^5) ~ GRE + TOEFL + LOR + CGPA + Research + 
                 I((TOEFL - mean(TOEFL))^2) + I((CGPA - mean(CGPA))^2) + I((CGPA - mean(CGPA))^3),
               data = Admission[-c( 66,  67  ,69, 116, 360), ],
               weights = w.i)

# Want to generate simultaneous CI's on coefficients, 9 counting intercept

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
alpha = 0.1/9

quantile(coeff_values["b0"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b1"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b2"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b3"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b4"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b5"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b22"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b44"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
quantile(coeff_values["b444"][,1], probs = c(alpha/2, 1 - alpha/2)) |> round(4)
