##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)            
## Researcher: Bobae Kang (github.com/bobaekang)                 
## Advisor: Benjamin Soltoff (github.com/bensoltoff)             
##------------------------------------------------------------------------------
## Script: thesis06_lab.R                                      
## Last updated: 7/1/17                                         
##------------------------------------------------------------------------------
## This script is a laboratory for analying prepared data and includes code for:
## 1. data preparation
## 2. PCA
## 3. binary logistic regression
## 4. multinomial logistic regression
##------------------------------------------------------------------------------
## This script requires the outputs from the following scripts:  
## * thesis01_prepare1.R
## * thesis02_prepare2.R
## * thesis03_sample_generation.R
##------------------------------------------------------------------------------

# setwd("~/UChicago/MA Thesis/R scripts") # for local use

set.seed(2017) # for reproducibility

# import pacakges
library(tidyverse)
library(feather)
library(broom)
library(coefplot)
library(GGally)
library(ggfortify)
library(car)
# library(mlogit) # for multinomial logit model: `mlogit()`
library(nnet) # for multinomial logit model: `multinom()`
library(stargazer)
library(lsmeans)
library(lmtest)


##------------------------------------------------------------------------------
## 1. Prepare data
##------------------------------------------------------------------------------

# import training data
divvy_from_train <- read_feather('data/divvy_from_train.feather')
divvy_to_train   <- read_feather('data/divvy_to_train.feather')

# fix mistake (per capita income)
cca_features     <- read_feather('data/cca_features.feather')

fixIncome <- function(x){
  output <- x %>%
    # remove the wrong values
    select(-income_per_capita) %>%
    # add the right values
    left_join(cca_features %>% select(cca_id, income_per_capita)) %>%
    # make cca_id as factors
    mutate(cca_id = as.factor(cca_id))

    return(output)
}

divvy_from_train <- fixIncome(divvy_from_train)
divvy_to_train   <- fixIncome(divvy_to_train)


# get dummies for subscriber and sub_male
generateVars <- function(x){
  output <- x %>%
    mutate(age_10 = (2016 - birthyear)/10,
           tripdistance_km = tripdistance/1000,
           subscriber = ifelse(usertype == "Subscriber", 1, 0))
           # sub_male = ifelse(gender == "Male" & is.na(gender) == FALSE, 1, 0))
  return(output)
}

from_train <- generateVars(divvy_from_train)
to_train   <- generateVars(divvy_to_train)


# get multinomial Y
generatePMM <- function(x){
  output <- x %>%
    mutate(pmm = ifelse(mm50 == 1, 'pmm1',
                        ifelse(mm100 ==1, 'pmm2',
                               ifelse(mm200 == 1, 'pmm3',
                                      ifelse(mm300 == 1, 'pmm4', 'none')))))
  output$pmm <- as.factor(output$pmm)
  return(output)
}

from_train <- generatePMM(from_train)
to_train <- generatePMM(to_train)


##------------------------------------------------------------------------------
## 2. PCA
##------------------------------------------------------------------------------

## 2.1. trips originating-------------------------------------------------------
from_train_for_pca <- from_train %>%
  dplyr::select(subscriber, weekday, startrush, startrushAM, PRCP, tripdistance_km,
         cbd, pop_density, perc_black, perc_employed, income_per_capita)
from_pca <- prcomp(from_train_for_pca, scale = TRUE)

# PCA plot
autoplot(from_pca, data = cbind(from_train_for_pca, pmm = from_train$pmm),
         colour = 'pmm', alpha = 0.4,
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  xlim(c(-0.04, 0.025)) +
  ylim(c(-0.04, 0.03)) +
  labs(title = 'Trips originating') +
  scale_color_hue(name = '',
                  labels = c('None', '0-50m', '50-100m', '100-200m', '200-300m')) +
  theme_minimal()
# ggsave('images/fig_pca_from.png')


## 2.2 trips terminating--------------------------------------------------------
to_train_for_pca <- to_train %>%
  dplyr::select(subscriber, weekday, stoprush, stoprushAM, PRCP, tripdistance_km,
         cbd, population, perc_black, perc_employed, income_per_capita)
to_pca <- prcomp(to_train_for_pca, scale = TRUE)

# PCA plot
autoplot(to_pca, data = cbind(to_train_for_pca, pmm = to_train$pmm),
         colour = 'pmm', alpha = 0.4,
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  xlim(c(-0.04, 0.025)) +
  ylim(c(-0.04, 0.03)) +
  labs(title = 'Trips terminating') +
  scale_color_hue(name = '',
                  labels = c('None', '0-50m', '50-100m', '100-200m', '200-300m')) +
  theme_minimal()
# ggsave('images/fig_pca_to.png')

cbind(from_pca$rotation[,1:2],to_pca$rotation[,1:2])
 

##------------------------------------------------------------------------------
## 3. Seperate binary logit models
##------------------------------------------------------------------------------
## 3.1. trips originating-------------------------------------------------------
# get formulas
getFormula <- function(y_var, x_vars){
  x_string <- paste(x_vars, collapse = ' + ')
  formula <- paste(c(y_var, x_string), collapse = ' ~ ')
  return(formula)
}

from_x_vars <- c('subscriber', 'weekday', 'startrush', 'startrushAM', 'tripdistance_km',
                 'PRCP', 'cbd', 'pop_density', 'perc_black', 'perc_employed', 'income_per_capita')

from50_formula <- getFormula('mm50', from_x_vars)
from100_formula <- getFormula('mm100', from_x_vars)
from200_formula <- getFormula('mm200', from_x_vars)
from300_formula <- getFormula('mm300', from_x_vars)

# fit models
from50_mod   <- glm(formula = from50_formula, data = from_train, family = binomial(link='logit'))
from100_mod  <- glm(formula = from100_formula, data = from_train, family = binomial(link='logit'))
from200_mod  <- glm(formula = from200_formula, data = from_train, family = binomial(link='logit'))
from300_mod  <- glm(formula = from300_formula, data = from_train, family = binomial(link='logit'))

# inspect results
# coefficients
tidy(from50_mod)  
tidy(from100_mod) 
tidy(from200_mod) 
tidy(from300_mod) 

# goodness of fit
glance(from50_mod)  
glance(from100_mod)
glance(from200_mod) 
glance(from300_mod)

# compare models
Anova(from50_mod)
Anova(from100_mod)
Anova(from200_mod)
Anova(from300_mod)

# compare coefficients visually
multiplot(from50_mod, from100_mod, from200_mod, from300_mod,
          title = "Trips originating",
          names = c('1: PMM1', '2: PMM2', '3: PMM3', '4: PMM4'),
          # predictors = c('pop_density_norm'),
          intercept = FALSE,
          dodgeHeight = 0.6,
          shape = 15:18,
          decreasing = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")
# # save plot
# ggsave('images/fig_coef_from.png')

## 3.2 trips terminating--------------------------------------------------------
# get formulas
to_x_vars <- c('subscriber', 'weekday', 'stoprush', 'stoprushAM', 'tripdistance_km',
                 'PRCP', 'cbd', 'pop_density', 'perc_black', 'perc_employed', 'income_per_capita')

to50_formula  <- getFormula('mm50', to_x_vars)
to100_formula <- getFormula('mm100', to_x_vars)
to200_formula <- getFormula('mm200', to_x_vars)
to300_formula <- getFormula('mm300', to_x_vars)

# fit models
to50_mod   <- glm(formula = to50_formula, data = to_train, family = binomial(link='logit'))
to100_mod  <- glm(formula = to100_formula, data = to_train, family = binomial(link='logit'))
to200_mod  <- glm(formula = to200_formula, data = to_train, family = binomial(link='logit'))
to300_mod  <- glm(formula = to300_formula, data = to_train, family = binomial(link='logit'))

# inspect results
# coefficients
tidy(to50_mod)  
tidy(to100_mod) 
tidy(to200_mod) 
tidy(to300_mod) 

# goodness of fit
glance(to50_mod)
glance(to100_mod)
glance(to200_mod)
glance(to300_mod)

# compare models
Anova(to50_mod)  
Anova(to100_mod) 
Anova(to200_mod) 
Anova(to300_mod) 

# compare coefficients visually
multiplot(to50_mod, to100_mod, to200_mod, to300_mod,
          title = "Trips terminating",
          names = c('1: PMM1', '2: PMM2', '3: PMM3', '4: PMM4'),
          # predictors = c('pop_density_norm'),
          intercept = FALSE,
          dodgeHeight = 0.6,
          shape = 15:18,
          decreasing = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")
# # save plot
# ggsave('images/fig_coef_to.png')


##------------------------------------------------------------------------------
## 4. Mulinomial logistic models
##------------------------------------------------------------------------------
# define a functon to get confidence interval (95%)
getConfint <- function(x){
  output <- x %>%1:10
    mutate(confint.l = estimate - 2*std.error,
           confint.h = estimate + 2*std.error)
  return(output)
}

# define a function to get coefficient estimates in odds ratio and log-odds ratio
# requires the getConfint() function defined above
getCoef <- function(model){
  output          <- tidy(model)
  output$estimate <- log(output$estimate) # because tidy gives estimates in odds, not log-odds
  output          <- getConfint(output)   # get confidence interval for log-odds ratio
  output          <- output %>%
    mutate(estimate.odds = exp(estimate),   # recover odds ratio
           confint.l.odds = exp(confint.l), # recover odds ratio interval lower bound
           confint.h.odds = exp(confint.h)) # recover odss ratio interval upper bound
  return(output)
}

# define a function to get a plot of model estimates
getPlot <- function(model, from = TRUE, log = TRUE){
  data <- getCoef(model) # get the table of coefficients
  if(from == TRUE){
    title = 'Trips originating'
  } else {
    title = 'Trips terminating'
  }
  if(log == TRUE){
    baseplot <- ggplot(data=data %>% filter(term != '(Intercept)'),
                       aes(x=term, y=estimate, ymin=confint.l, ymax=confint.h,
                           group = y.level, color = y.level))
    yintercept <- 0
    ylimit <- c(-0.5, 1.5)
  } else {
    baseplot <- ggplot(data=data %>% filter(term != '(Intercept)'),
                       aes(x=term, y=estimate.odds, ymin=confint.l.odds, ymax=confint.h.odds,
                           group = y.level, color = y.level))
    yintercept <- 1
    ylimit <- c(0, 4.5)
  }
  baseplot +
    geom_point(aes(shape = y.level), position=position_dodge(0.6), size=2) +
    geom_errorbar(position=position_dodge(0.6), width = 1) +
    geom_hline(yintercept = yintercept, linetype = 'dashed', color = 'darkgrey') +
    labs(title = title, x = 'Coefficient', y = 'Value') +
    scale_color_hue(name = '', labels = c('1: PMM1', '2: PMM2', '3: PMM3', '4: PMM4')) +
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    guides(shape=FALSE) +
    ylim(ylimit) +
    coord_flip()
}


## 4.1. trips originating-------------------------------------------------------
# fit model
from_formula <- getFormula('pmm', from_x_vars)
# multinom
from_mod_M <- multinom(formula = from_formula, data = from_train, Hess = TRUE)

# inspect results
summary(from_mod_M)
tidy(from_mod_M, summary(from_mod_M)$standard.errors) %>% arrange(term)
glance(from_mod_M)
exp(coef(from_mod_M))

# # chi-square test: null = no differences in pmm categories
# chisq_from <- deviance(multinom(pmm~1, data=from_train)) - deviance(from_mod_M)
# df_from <- (nrow(from_train)-11)*(length(unique(from_train$pmm))-1)
# pchisq(chisq_from, df_from)
# 
# Anova/likelihood ratio test
anova(multinom(pmm~1, data=from_train), from_mod_M) # for the entire model
Anova(from_mod_M) # for each term; equivalent to lrtest for each term

# for table
coef(from_mod_M) %>% t() # estimates in log-odds ratio
tidy(from_mod_M) %>% dplyr::select(y.level, term, estimate) %>% spread(y.level, estimate) # estimates in relative risk
tidy(from_mod_M) %>% dplyr::select(y.level, term, std.error) %>% spread(y.level, std.error) # standard error
tidy(from_mod_M) %>% dplyr::select(y.level, term, p.value) %>% spread(y.level, p.value) # p-value

# compare coefficients visually
from_coef_plot <- getPlot(from_mod_M, from = TRUE, log = TRUE) # in log-odds ratio

# for paper
from_coef_plot +
  theme_minimal() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_coef_from.png', width=7.5, height=5)

# # for poster
# from_coef_plot +
#   theme_bw() +
#   theme(legend.position = "bottom")
# # save plot
# ggsave('images/fig_coef_from_poster.png', width=7, height=4)

# get predicted probabilities
from_newdata1 <- data.frame(subscriber = rep(c(1,0), each=151*4),
                           weekday = 1,
                           startrush = c(rep(c(1,0), each=151*2), rep(c(1,0), each=151*2)),
                           startrushAM = 1,
                           tripdistance_km = rep(seq(0, 15, 0.1), 8),
                           PRCP = 1,
                           cbd = c(c(rep(c(1,0), each=151), rep(c(1,0), each=151)),
                                   c(rep(c(1,0), each=151), rep(c(1,0), each=151))),
                           pop_density = median(from_train$pop_density),
                           perc_black = median(from_train$perc_black),
                           perc_employed = median(from_train$perc_employed),
                           income_per_capita = median(from_train$income_per_capita))
from_pred1 <- cbind(from_newdata1,
                    predict(from_mod_M, newdata = from_newdata1, type = "probs", se = TRUE))
from_predprobs1 <- reshape2::melt(from_pred1[, c(1,3,5,7,12:16)],
                                  id.vars = c("subscriber", "startrush", "tripdistance_km", "cbd"),
                                  value.name = "probability")
from_predprobs1$startrush  <- ifelse(from_predprobs1$startrush == 1, '1: Rush hour', '0: Other time')
from_predprobs1$cbd        <- ifelse(from_predprobs1$cbd == 1, '1: CBD', '0: Other area')

# plot predicted probabilities
from_predprobs1 %>%
  filter(subscriber==1) %>%
  ggplot(., aes(x = tripdistance_km, y = probability, colour=variable))+
  geom_line() +
  facet_wrap(~ startrush + cbd, ncol=1) +
  scale_colour_hue(name="") +
  ylim(c(0,.8)) +
  labs(title = 'Trips originating: Subscriber',
       x = 'Trip distance (km)',
       y = 'Predicted probability') +
  theme_minimal() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_prob_from_subscriber.png') # for paper
ggsave('images/fig_prob_from_subscriber.png', width=4, height=12) # for poster

from_predprobs1 %>%
  filter(subscriber==0) %>%
  ggplot(., aes(x = tripdistance_km, y = probability, colour=variable))+
  geom_line() +
  facet_wrap(~ startrush + cbd, ncol=1) +
  scale_colour_hue(name="") +
  ylim(c(0,.8)) +
  labs(title = 'Trips originating: Customer',
       x = 'Trip distance (km)',
       y = 'Predicted probability') +
  theme_minimal() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_prob_from_subscriber.png') # for paper
ggsave('images/fig_prob_from_customer.png', width=4, height=12)

 
## 4.2 trips terminating--------------------------------------------------------
# fit model
to_formula <- getFormula('pmm', to_x_vars)
to_mod_M <- multinom(formula = to_formula, data = to_train)

# inspect results
summary(to_mod_M)
tidy(to_mod_M) %>% arrange(term)
glance(to_mod_M)
exp(coef(to_mod_M))

# # chi-square test: null = no differences in pmm categories
# chisq_to <- deviance(multinom(pmm~1, data=to_train)) - deviance(to_mod_M)
# df_to <- (nrow(from_train)-11)*(length(unique(from_train$pmm))-1)
# pchisq(chisq_to, df_to)

# Anova/likelihood ratio test of each term
anova(multinom(pmm~1, data=to_train), to_mod_M) # for the entire model
Anova(to_mod_M) # equivalent to lrtest for each term

# for table
coef(to_mod_M) %>% t() # in log-odds ratio
tidy(to_mod_M) %>% dplyr::select(y.level, term, estimate) %>% spread(y.level, estimate) # in odds ratio
tidy(to_mod_M) %>% dplyr::select(y.level, term, std.error) %>% spread(y.level, std.error)
tidy(to_mod_M) %>% dplyr::select(y.level, term, p.value) %>% spread(y.level, p.value)

# compare coefficients (in log) visually
to_coef_plot <- getPlot(to_mod_M, from = FALSE, log = TRUE) # in log-odds ratio 

# for paper
to_coef_plot +
  theme_minimal() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_coef_to.png', width=7.5, height=5) # for paper

# for poster
to_coef_plot +
  theme_bw() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_coef_to_poster.png', width=7, height=4)

# get predicted probabilities
to_newdata1 <- data.frame(subscriber = rep(c(1,0), each=151*4),
                          weekday = 1,
                          stoprush = c(rep(c(1,0), each=151*2), rep(c(1,0), each=151*2)),
                          stoprushAM = 1,
                          tripdistance_km = rep(seq(0, 15, 0.1), 8),
                          PRCP = 1,
                          cbd = c(c(rep(c(1,0), each=151), rep(c(1,0), each=151)),
                                  c(rep(c(1,0), each=151), rep(c(1,0), each=151))),
                          pop_density = median(to_train$pop_density),
                          perc_black = median(to_train$perc_black),
                          perc_employed = median(to_train$perc_employed),
                          income_per_capita = median(to_train$income_per_capita))
to_pred1 <- cbind(to_newdata1,
                    predict(to_mod_M, newdata = to_newdata1, type = "probs", se = TRUE))
to_predprobs1 <- reshape2::melt(to_pred1[, c(1,3,5,7,12:16)],
                                  id.vars = c("subscriber", "stoprush", "tripdistance_km", "cbd"),
                                  value.name = "probability")
to_predprobs1$stoprush  <- ifelse(to_predprobs1$stoprush == 1, '1: Rush hour', '0: Other time')
to_predprobs1$cbd       <- ifelse(to_predprobs1$cbd == 1, '1: CBD', '0: Other area')

# plot predicted probabilities
to_predprobs1 %>%
  filter(subscriber==1) %>%
  ggplot(., aes(x = tripdistance_km, y = probability, colour=variable))+
  geom_line() +
  facet_wrap(~ stoprush + cbd, ncol=1) +
  scale_colour_hue(name="") +
  ylim(c(0,.8)) +
  labs(title = 'Trips terminating: Subscriber',
       x = 'Trip distance (km)',
       y = 'Predicted probability') +
  theme_minimal() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_prob_to_subscriber.png', width=4, height=12)

to_predprobs1 %>%
  filter(subscriber==0) %>%
  ggplot(., aes(x = tripdistance_km, y = probability, colour=variable))+
  geom_line() +
  facet_wrap(~ stoprush + cbd, ncol=1) +
  scale_colour_hue(name="") +
  ylim(c(0,.8)) +
  labs(title = 'Trips terminating: Customer',
       x = 'Trip distance (km)',
       y = 'Predicted probability') +
  theme_minimal() +
  theme(legend.position = "bottom")
# save plot
# ggsave('images/fig_prob_to_customer.png', width=4, height=12)


# 4.3. Trips originating, with different baseline category----------------------
getNewBaseline <- function(data, base){
  labels <- c('none', 'pmm1', 'pmm2', 'pmm3', 'pmm4')
  not_base <- labels[labels != base]
  output <- ifelse(data$pmm == base, '0',
                   ifelse(data$pmm == not_base[1], '1',
                          ifelse(data$pmm == not_base[2], '2',
                                 ifelse(data$pmm == not_base[3], '3', '4')))) %>% as.factor()
  return(output)  
}

# modify the pmm column
from_pmm_base50 <- getNewBaseline(from_train, 'pmm1')
from_pmm_base100 <- getNewBaseline(from_train, 'pmm2')
from_pmm_base200 <- getNewBaseline(from_train, 'pmm3')
from_pmm_base300 <- getNewBaseline(from_train, 'pmm4')

from_train_base50  <- from_train %>% select(-pmm)
from_train_base100 <- from_train %>% select(-pmm)
from_train_base200 <- from_train %>% select(-pmm)
from_train_base300 <- from_train %>% select(-pmm)

from_train_base50$pmm  <- from_pmm_base50
from_train_base100$pmm <- from_pmm_base100
from_train_base200$pmm <- from_pmm_base200
from_train_base300$pmm <- from_pmm_base300

# fit multinomial model using different baseline categories
from_mod_M_base50  <- multinom(formula = from_formula, data = from_train_base50, Hess = TRUE)
from_mod_M_base100 <- multinom(formula = from_formula, data = from_train_base100, Hess = TRUE)
from_mod_M_base200 <- multinom(formula = from_formula, data = from_train_base200, Hess = TRUE)
from_mod_M_base300 <- multinom(formula = from_formula, data = from_train_base300, Hess = TRUE)

# check the coefficients
stargazer(from_mod_M_base50, type = "text")
stargazer(from_mod_M_base100, type = "text")
stargazer(from_mod_M_base200, type = "text")
stargazer(from_mod_M_base300, type = "text")

# check the coefficients in probabilities
coef(from_mod_M_base50) %>% exp() %>% t()
coef(from_mod_M_base100) %>% exp() %>% t()
coef(from_mod_M_base200) %>% exp() %>% t()
coef(from_mod_M_base300) %>% exp() %>% t()


# 4.4. Trips terminating, with different baseline category----------------------
# modify the pmm column
to_pmm_base50 <- getNewBaseline(to_train, 'pmm1')
to_pmm_base100 <- getNewBaseline(to_train, 'pmm2')
to_pmm_base200 <- getNewBaseline(to_train, 'pmm3')
to_pmm_base300 <- getNewBaseline(to_train, 'pmm4')

to_train_base50  <- to_train %>% select(-pmm)
to_train_base100 <- to_train %>% select(-pmm)
to_train_base200 <- to_train %>% select(-pmm)
to_train_base300 <- to_train %>% select(-pmm)

to_train_base50$pmm  <- to_pmm_base50
to_train_base100$pmm <- to_pmm_base100
to_train_base200$pmm <- to_pmm_base200
to_train_base300$pmm <- to_pmm_base300

# fit multinomial model using different baseline categories
to_mod_M_base50  <- multinom(formula = to_formula, data = to_train_base50, Hess = TRUE)
to_mod_M_base100 <- multinom(formula = to_formula, data = to_train_base100, Hess = TRUE)
to_mod_M_base200 <- multinom(formula = to_formula, data = to_train_base200, Hess = TRUE)
to_mod_M_base300 <- multinom(formula = to_formula, data = to_train_base300, Hess = TRUE)

# check the coefficients
stargazer(to_mod_M_base50, type = "text")
stargazer(to_mod_M_base100, type = "text")
stargazer(to_mod_M_base200, type = "text")
stargazer(to_mod_M_base300, type = "text")

# check the coefficients in probabilities
coef(to_mod_M_base50) %>% exp() %>% t()
coef(to_mod_M_base100) %>% exp() %>% t()
coef(to_mod_M_base200) %>% exp() %>% t()
coef(to_mod_M_base300) %>% exp() %>% t()


# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment
