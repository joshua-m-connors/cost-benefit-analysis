# Populate the variables below for the various PERT distributions
# Probability That Initiative Succeeds (Min, Likely, Max, Confidence)
cb_prob_min <- 0.01
cb_prob_likely <- 0.2
cb_prob_max <- 0.3
cb_prob_conf <- 2
# Benefits Annual Rate of Occurrence (Min, Likely, Max, Confidence)
b_aro_min <- 500
b_aro_likely <- 1000
b_aro_max <- 5000
b_aro_conf <- 2
# Benefits Primary Single Gain Expectancy (Min, Likely, Max, Confidence)
b_psge_min <- 5
b_psge_likely <- 25
b_psge_max <- 75
b_psge_conf <- 2
# Fixed Gain Amount (If Applicable)
fg <- 0
# Costs Annual Rate of Occurrence (Min, Likely, Max, Confidence)
c_aro_min <- 0.01
c_aro_likely <- 1
c_aro_max <- 2
c_aro_conf <- 4
# Costs Primary Single Loss Expectancy (Min, Likely, Max, Confidence)
c_psle_min <- 5000
c_psle_likely <- 100000
c_psle_max <- 500000
c_psle_conf <- 4
# Fixed Cost Amount (If Applicable)
fc <- 5000
# How Many Simulations?
n <- 100000

# Only edit below this line to change logic
#-----------------------------------------------------------------------

library(mc2d)
library(ggplot2)

set.seed(88881111)
# Probability of Success
cb_prob <- rpert(n, cb_prob_min, cb_prob_likely, cb_prob_max, shape = cb_prob_conf)
cb_prob_mean <- mean(cb_prob)
cb_prob_inv <- rpert(n, (1 - cb_prob_min), (1 - cb_prob_likely), (1 - cb_prob_max), shape = cb_prob_conf)
# Benefit Calculate Annual Rate of Occurrence
b_aro <- rpert(n, b_aro_min, b_aro_likely, b_aro_max, shape = b_aro_conf)
b_psge <- rpert(n, b_psge_min, b_psge_likely, b_psge_max, shape = b_psge_conf)
# Cost Calculate Annual Rate of Occurrence
c_aro <- rpert(n, c_aro_min, c_aro_likely, c_aro_max, shape = c_aro_conf)
c_psle <- rpert(n, c_psle_min, c_psle_likely, c_psle_max, shape = c_psle_conf)


# Annualized Gain = b_aro * b_psge
b_age <- ((b_aro * cb_prob) * b_psge) + fg
# Annualized Loss = c_aro * c_psle
c_ale <- ((c_aro * cb_prob_inv) * c_psle) + fc
c_ale <- c_ale * -1

ae <- b_age + c_ale

# That's it! Now let's plot it. Need 'scales' to show commas on x-axis.
library(scales)
gg <- ggplot(data.frame(ae), aes(x = ae))
gg <- gg + geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 10000)
gg <- gg + geom_density(fill = "steelblue", alpha = 1/3)
gg <- gg + scale_x_continuous(labels = comma)
gg <- gg + theme_bw()
print(gg)

### Probability of Succeeding ###
summary(cb_prob)
### Probability of Not Succeeding ###
summary(cb_prob_inv)
### Benefits ###
# Benefit Annualized Rate of Occurrence
summary(b_aro)
# Benefit Single Gain Expectancy
dollar(c(summary(b_psge)))
# Benefit Annualized Gain Expectancy
dollar(c(summary(b_age)))
### Costs ###
# Cost Annualized Rate of Occurrence
summary(c_aro)
# Cost Single Loss Expectancy
dollar(c(summary(c_psle)))
# Cost Annualized Loss Expectancy
dollar(c(summary(c_ale)))
#Combined Distribution
dollar(c(summary(ae)))

dollar(quantile(ae, probs = c(0.1, 0.25, 0.5, 0.75, 0.85, 0.9, 0.95, 0.99, 0.9999)))
