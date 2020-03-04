## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Reading data
library(foreign)

# Required packages
library(magrittr) # for pipes
library(nnet) # for the multinom()-function
library(MASS) # for the multivariate normal distribution

# The package
# devtools::install_github("ManuelNeumann/MNLpred")
library(MNLpred)

# Plotting the predicted probabilities:
library(ggplot2)
library(scales)

## ----data---------------------------------------------------------------------
# The data:
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

## ----data_preparation---------------------------------------------------------
# Data preparation:

# Set "academic" as the reference category for the multinomial model
ml$prog2 <- relevel(ml$prog, ref = "academic")

# Computing a numeric dummy for "female" (= 1)
ml$female2 <- as.numeric(ml$female == "female")

## ----model--------------------------------------------------------------------
# Multinomial logit model:
mod1 <- multinom(prog2 ~ female2 + read + write + math + science,
                 Hess = TRUE,
                 data = ml)

## ----results------------------------------------------------------------------
summary(mod1)

## ----math---------------------------------------------------------------------
summary(ml$math)

## ----mnl_pred_ova-------------------------------------------------------------
pred1 <- mnl_pred_ova(model = mod1,
                      data = ml,
                      xvari = "math",
                      by = 1,
                      seed = "random", # default
                      nsim = 100, # faster than the default 1000
                      probs = c(0.025, 0.975)) # default

## ----return-------------------------------------------------------------------
pred1$plotdata %>% head()

## ----prediction_plot1---------------------------------------------------------
ggplot(data = pred1$plotdata, aes(x = math, y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line() + # Mean
  facet_grid(prog2 ~., scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Math score") # Always label your axes ;)

## ----static_fd----------------------------------------------------------------
fdif1 <- mnl_fd2_ova(model = mod1,
                     data = ml,
                     xvari = "math",
                     value1 = min(ml$math),
                     value2 = max(ml$math),
                     nsim = 100)

## ----static_fd_plot-----------------------------------------------------------
ggplot(fdif1$plotdata_fd, aes(categories, y = mean,
                              ymin = lower, max = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format(),
                     name = "First difference") +
  theme_bw()

## ----first_diffferences_prediction--------------------------------------------
fdif1 <- mnl_fd_ova(model = mod1,
                    data = ml,
                    xvari = "math",
                    by = 1,
                    scenname = "female2",
                    scenvalues = c(0,1),
                    nsim = 100)

## ----fd_return----------------------------------------------------------------
fdif1$plotdata %>% head()

## ----prediction_plot2---------------------------------------------------------
pred_plotdat <- rbind(fdif1$Prediction1$plotdata,
                      fdif1$Prediction2$plotdata)

ggplot(data = pred_plotdat, aes(x = math, y = mean,
                                ymin = lower, ymax = upper,
                                linetype = as.factor(female2))) +
  geom_ribbon(alpha = 0.1) +
  geom_line() +
  facet_grid(prog2 ~., scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  scale_linetype_discrete(name = "Female") +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Math score") # Always label your axes ;)

## ----first_differences_plot---------------------------------------------------
ggplot(data = fdif1$plotdata_fd, aes(x = math, y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_grid(prog2 ~., scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # % labels
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Math score") # Always label your axes ;)

