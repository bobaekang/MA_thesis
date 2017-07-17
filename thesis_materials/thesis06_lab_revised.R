##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)            
## Researcher: Bobae Kang (github.com/bobaekang)                 
## Advisor: Benjamin Soltoff (github.com/bensoltoff)             
##------------------------------------------------------------------------------
## Script: thesis06_lab.R                                      
## Last updated: 7/17/17                                         
##------------------------------------------------------------------------------
## This script is a laboratory for analying prepared data and includes code for:
## 1. data preparation
## 2. PCA
## 3. binary logit models for nested dichotomies
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
library(coefplot)?
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


##------------------------------------------------------------------------------
## 3. Seperate binary logit models for all nested dichotomies 
##------------------------------------------------------------------------------
# define useful functions
getFormula <- function(y_var, x_vars){
  x_string <- paste(x_vars, collapse = ' + ')
  formula <- paste(c(y_var, x_string), collapse = ' ~ ')
}

getResults <- function(mod1, mod2, mod3, mod4, item='coef', round=NULL){
  if(item == 'coef') col_num <- 2
  else if (item == 'se') col_num <-3
  else if (item == 'pval') col_num <- 5
  else stop()

  term  <- tidy(mod1)$term
  nest1 <- tidy(mod1)[[col_num]]
  nest2 <- tidy(mod2)[[col_num]]
  nest3 <- tidy(mod3)[[col_num]]
  nest4 <- tidy(mod4)[[col_num]]
  df <- data.frame(term, nest1, nest2, nest3, nest4)

  if(is.null(round)) df
  else cbind(df[1], round(df[2:5], round))
}


## 3.1. trips originating-------------------------------------------------------
# prepare data
new_x_names <- c('Subscriber', 'Trip_distance', 'Precipitation', 'Weekday', 'Rushhour', 'Rush_hour_AM',
                 'Station_in_CBD', 'Population_density', 'Black', 'Employed', 'Income_per_capita')
colnames(from_train)[c(54, 53, 31, 24, 25, 27, 36, 39, 40, 41, 51)] <- new_x_names
from_train <- from_train %>%
  select(new_x_names, mm300, mm200, mm100, mm50)

# get formulas
formula_nest1 <- getFormula('mm300', new_x_names)
formula_nest2 <- getFormula('mm200', new_x_names)
formula_nest3 <- getFormula('mm100', new_x_names)
formula_nest4 <- getFormula('mm50', new_x_names)

# fit models
from_mod_nest1 <- glm(formula = formula_nest1, data = from_train, family = binomial(link='logit'))
from_mod_nest2 <- glm(formula = formula_nest2, data = from_train[from_train$mm300 == 1,], family = binomial(link='logit'))
from_mod_nest3 <- glm(formula = formula_nest3, data = from_train[from_train$mm200 == 1,], family = binomial(link='logit'))
from_mod_nest4 <- glm(formula = formula_nest4, data = from_train[from_train$mm100 == 1,], family = binomial(link='logit'))

# inspect results
(getResults(from_mod_nest1, from_mod_nest2, from_mod_nest3, from_mod_nest4, item='coef', round=4))
(getResults(from_mod_nest1, from_mod_nest2, from_mod_nest3, from_mod_nest4, item='se', round=4))
(getResults(from_mod_nest1, from_mod_nest2, from_mod_nest3, from_mod_nest4, item='pval', round=2))

# plot coefficients
multiplot(from_mod_nest1, from_mod_nest2, from_mod_nest3, from_mod_nest4,
          title = "Trips originating",
          names = c('Dichotomy 1', 'Dichotomy 2', 'Dichotomy 3', 'Dichotomy 4'),
          intercept = FALSE,
          dodgeHeight = -0.6,
          shape = 15:18,
          decreasing = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")
# # save plot
# ggsave('images/fig_coef_from.png', width=8, height=6)


## 3.2 trips terminating--------------------------------------------------------
# change variable names
colnames(to_train)[c(54, 24, 25, 27, 53, 31, 36, 39, 40, 41, 51)] <- new_x_names
to_train <- to_train %>%
  select(new_x_names, mm300, mm200, mm100, mm50)

# fit models
to_mod_nest1 <- glm(formula = formula_nest1, data = to_train, family = binomial(link='logit'))
to_mod_nest2 <- glm(formula = formula_nest2, data = to_train[to_train$mm300 == 1,], family = binomial(link='logit'))
to_mod_nest3 <- glm(formula = formula_nest3, data = to_train[to_train$mm200 == 1,], family = binomial(link='logit'))
to_mod_nest4 <- glm(formula = formula_nest4, data = to_train[to_train$mm100 == 1,], family = binomial(link='logit'))

# inspect results
(getResults(to_mod_nest1, to_mod_nest2, to_mod_nest3, to_mod_nest4, item='coef', round=5))
(getResults(to_mod_nest1, to_mod_nest2, to_mod_nest3, to_mod_nest4, item='se', round=5))
(getResults(to_mod_nest1, to_mod_nest2, to_mod_nest3, to_mod_nest4, item='pval', round=2))

# plot coefficients
multiplot(to_mod_nest1, to_mod_nest2, to_mod_nest3, to_mod_nest4,
          title = "Trips terminating",
          names = c('Dichotomy 1', 'Dichotomy 2', 'Dichotomy 3', 'Dichotomy 4'),
          intercept = FALSE,
          dodgeHeight = -0.6,
          shape = 15:18,
          decreasing = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")
# # save plot
# ggsave('images/fig_coef_to.png', width=8, height=6)


# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment
