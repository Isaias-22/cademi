library(RCurl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(psych)

##
data <- getURL("https://raw.githubusercontent.com/PabloFdezm/cademi/master/new_data.csv?token=AKX6XZNCMS255MOH7SZKJEK7MPYYG")
data <- read.csv(text= data, row.names = 1)
##

###################################### Psysafety #################################################

psysafe <- data %>%
  select(starts_with("psy"))

eliminacion <- 0

while ( eliminacion >= 0.70 ) {
  
  alfa <- alpha(psysafe)
  alfa_raw <- alfa$total$raw_alpha
  
  alfa_drop <- alfa$alpha.drop %>%
    as.data.frame()
  
  # Strategy
  eliminacion <- alfa_drop %>%
    select(raw_alpha)%>%
    filter(alfa_raw < raw_alpha)%>%
    row.names()%>%
    head(1)
  psysafe <- psysafe %>%
    select(-eliminacion)
  #
  
}

psysafe_select <- colnames(psysafe)

psysafe <- data %>%
  select(equipoID, all_of(psysafe_select))%>%
  pivot_longer(-equipoID)%>%
  group_by(equipoID)%>%
  summarize(psysafe=round(mean(value),2))

###################################### Learn #################################################

learnb <- data %>%
  select(starts_with("learn"))

eliminacion <- 0

while ( eliminacion >= 0.7 ) {
  
  alfa <- alpha(learnb)
  alfa_raw <- alfa$total$raw_alpha
  
  alfa_drop <- alfa$alpha.drop %>%
    as.data.frame()
  
  # Strategy
  eliminacion <- alfa_drop %>%
    select(raw_alpha)%>%
    filter(alfa_raw < raw_alpha)%>%
    row.names()%>%
    head(1)
  learnb <- learnb %>%
    select(-eliminacion)
  #
  
}

learnb_select <- colnames(learnb)

learnb <- data %>%
  select(equipoID, all_of(learnb_select))%>%
  pivot_longer(-equipoID)%>%
  group_by(equipoID)%>%
  summarize(learnbehavior=round(mean(value),2))

###################################### Efficacy #################################################

efficacy <- data %>%
  select(starts_with("eff"))

eliminacion <- 0

while ( eliminacion >= 0.7 ) {
  
  alfa <- alpha(efficacy)
  alfa_raw <- alfa$total$raw_alpha
  
  alfa_drop <- alfa$alpha.drop %>%
    as.data.frame()
  
  # Strategy
  eliminacion <- alfa_drop %>%
    select(raw_alpha)%>%
    filter(alfa_raw < raw_alpha)%>%
    row.names()%>%
    head(1)
  efficacy <- efficacy %>%
    select(-eliminacion)
  #
  
}
efficacy_select <- colnames(efficacy)

efficacy <- data %>%
  select(equipoID, all_of(efficacy_select) )%>%
  pivot_longer(-equipoID)%>%
  group_by(equipoID)%>%
  summarize(efficacy=round(mean(value),2))

###################################### Team Satisfaction #################################################

team_s <- data %>%
  select(starts_with("team"))

eliminacion <- 0

while ( eliminacion >= 0.7 ) {
  
  alfa <- alpha(team_s)
  alfa_raw <- alfa$total$raw_alpha
  
  alfa_drop <- alfa$alpha.drop %>%
    as.data.frame()
  
  # Strategy
  eliminacion <- alfa_drop %>%
    select(raw_alpha)%>%
    filter(alfa_raw < raw_alpha)%>%
    row.names()%>%
    head(1)
  team_s <- team_s %>%
    select(-eliminacion)
  #
  
}
team_s_select <- colnames(team_s)

team_s <- data %>%
  select(equipoID, all_of(team_s_select) )%>%
  pivot_longer(-equipoID)%>%
  group_by(equipoID)%>%
  summarize(teamsatisfaction=round(mean(value),2))

###################################### Performance #################################################

performance <- data %>%
  select(equipoID, starts_with("per"))%>%
  pivot_longer(-equipoID)%>%
  group_by(equipoID)%>%
  summarize(performance=round(mean(value),2))


data_2 <- full_join(performance, psysafe)
data_2 <- full_join(data_2,team_s)
data_2 <- full_join(data_2,learnb)
data_2 <- full_join(data_2,efficacy)

model_1 <- lm(performance ~ learnbehavior, data_2)
summary(model_1)

model_2 <- lm(performance ~ psysafe, data_2)
summary(model_2)

model_3 <- lm(performance ~ efficacy, data_2)
summary(model_3)


