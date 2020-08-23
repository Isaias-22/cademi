library(RCurl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(hrbrthemes, gganimate, ggcorrplot, plotly)

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
  theme(axis.text.x = element_text(angle=90))

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

cor(data)%>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, 
                         type = "lower", 
                         lab = TRUE, 
                         lab_size = 2, 
                         method="circle", 
                         colors = c("tomato2", "white", "springgreen3"), 
                         title="Correlaciones", 
                         ggtheme=theme_bw)

data_1 <- data%>%
  select(equipoID, teamsatisfaction1, teamsatisfaction2)%>%
  mutate(class = ifelse(teamsatisfaction1 - teamsatisfaction2 < 0, "red", "green"))

ifelse(data_1$class == "red", paste(data_1$equipoID), 0)

left_label <- paste(data_1$equipoID)
right_label <- paste(data_1$equipoID)

p <-  ggplot(data_1) + 
  geom_segment(aes(x=1, xend=2, y=`teamsatisfaction1`, yend=`teamsatisfaction2`, col=class), 
               size=.75, show.legend=F)+
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +
  labs(x="", y="Team Satisfaction") +  
  xlim(0, 2) + ylim(1,5)

p <- p + geom_text(label=left_label, y=data_1$teamsatisfaction1, x=rep(1, NROW(data_1)), hjust=2, size=2, angle=20)
p
