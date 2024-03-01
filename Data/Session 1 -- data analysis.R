## needed library to manipulate data
library(tidyverse) 
library(broom)

## data import
df <- read_csv("Data/Session_1_BDM_in_action_data.csv")

## let's run some tests!

## test #1: is there an effect on period 1 of BEING OBSERVED in the future. 
df %>% 
  filter(period == 1) %>% 
  group_by(product) %>% 
  group_modify(~tidy(t.test(.$bid~.$control)))
## NO EFFECT exactly as we thought. Good!

## test #2: is there convergence? and if so, towards high, middle, low
## predictions: Vicktoria YES, upwards
##              Ericka YAS, towards the middle
df %>% 
  filter(period <= 4) %>% 
  filter(product == "chocolate") %>% 
  group_by(period) %>% 
  summarise(mean_bid = mean(bid), sd_bid = sd(bid))

## test #3: does giving AB organic info impact WTP? for chocolate? for wine? for both? 
## predictions: positive impact; Giulia: higher for wine; 
df %>% 
  filter(period == 4 | period == 5) %>% 
  group_by(product) %>% 
  group_modify(~tidy(t.test(.$bid~.$period)))





