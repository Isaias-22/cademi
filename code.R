library(RCurl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(hrbrthemes, gganimate)

data <- getURL("https://raw.githubusercontent.com/PabloFdezm/cademi/master/data.csv?token=AKX6XZP5VWIKIMLSZYTNRV27JBDCS")
data <- read.csv(text= data, row.names = 1)

data%>%
  pivot_longer(-equipoID, names_to = "detail", values_to = "value")%>%
  group_by(detail)%>%
  summarize(average = mean(value))%>%
  ggplot(aes(x=average,y=detail))+
  geom_col()+
  labs(x="Promedio",
       y="",
       title= "Promedio de los valores medidos")+
  theme(axis.text.x = element_text(angle=90))+
  theme_tufte()

data %>%
  select(equipoID, starts_with("pe"))%>%
  pivot_longer(-equipoID, names_to="detail", values_to = "value")%>%
  group_by(detail)%>%
  summarize(mean= mean(value), median=median(value))%>%
  ggplot()+
  geom_point(aes(x=detail,y=mean,color="Promedio"))+
  geom_point(aes(x=detail,y=median,color="Mediana"))+
  scale_y_continuous(limits = c(0,4))+
  labs(title="Promedio vs Mediana",
       x="",
       y="Promedio",
       colour=" ")+
  hrbrthemes::theme_ipsum()+
  theme(axis.text.x = element_text(angle=10))
  
data %>%
  select(equipoID, starts_with("pe"))%>%
  pivot_longer(-equipoID, names_to = "detail", values_to = "value")%>%
  ggplot()+
  geom_density(aes(x=value, fill=detail),alpha=0.3)+
  labs(title = "Density by Performance",
       x=" ",
       y="Density",
         fill="Fill:")+
  hrbrthemes::theme_ipsum()

data %>%
  select(equipoID, ends_with("1"))%>%
  pivot_longer(-equipoID, names_to = "detail", values_to = "value")%>%
  ggplot()+
  geom_density(aes(x=value, fill=detail))+
  facet_wrap(~ detail)+
  hrbrthemes::theme_tinyhand()+
  labs(title = "Density Facet by Detail")

data %>%
  select(equipoID, starts_with("psy"))%>%
  pivot_longer(-equipoID, names_to = "detail", values_to = "value")%>%
  group_by(detail)%>%
  summarize(mean = mean(value))%>%
  ggplot(aes(x=detail, y=mean))+
  geom_col()+
  labs(title="Promedio de Seguridad Psicologica",
       x="",
       y="Promedio")+
  hrbrthemes::theme_ipsum()
