### Librerias

library(RCurl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

### Data

data <- getURL("https://raw.githubusercontent.com/PabloFdezm/cademi/master/data.csv?token=AKX6XZNXLLLB4KGPTP6ICUC7LWLHY")
data <- read.csv(text= data, row.names = 1)

### Recode: Psycafe 1, psycafe 3, psycafe 5, ts2.

data <- data %>%
  mutate(equipoID = as.factor(equipoID),
         psycsafe1 = 5 - (psycsafe1 -1),
         psycsafe3 = 5 - (psycsafe3 -1),
         psycsafe5 = 5 - (psycsafe5 -1),
         teamteamsatisfaction2 = 5 - (teamsatisfaction2 -1))

## Visualization

data %>%
  select(equipoID, starts_with("psy")) %>%
  mutate(equipoID = as.factor(equipoID))%>%
  pivot_longer(-equipoID)%>%
  ggplot()+
  geom_point(aes(x=equipoID,y=value, color=name))+
  facet_wrap(~name)+
  ggplot2::theme_classic()+
  theme(axis.text.x = element_text(angle = 45, size = 5))

## Aggregation of psysafety

psysafety <- data %>%
  pivot_longer(-equipoID, names_to = "detail", values_to = "values")%>%
  transmute(equipoID, detail, values = if_else( grepl("psy", detail), values, 0))%>%
  filter(values != 0)%>%
  group_by(equipoID)%>%
  summarize(psysafety = round(mean(values),2))

