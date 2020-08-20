library(RCurl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)

data <- getURL("https://raw.githubusercontent.com/PabloFdezm/cademi/master/data.csv?token=AKX6XZP5VWIKIMLSZYTNRV27JBDCS")
data <- read.csv(text= data)
data <- data[,-1]

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
  theme(axis.text.x = element_text(angle=90))+
  labs(title="Promedio vs Mediana",
       x="",
       y="Promedio",
       colour=" ")+
  theme_stata()
