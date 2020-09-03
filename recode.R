### Librerias

library(RCurl)
library(dplyr)

### Data

data <- getURL("https://raw.githubusercontent.com/PabloFdezm/cademi/master/data.csv?token=AKX6XZP5VWIKIMLSZYTNRV27JBDCS")
data <- read.csv(text= data, row.names = 1)

### Recode: Psycafe 1, psycafe 3, psycafe 5, ts2.

data %>%
  mutate(psycsafe1 = case_when(psycsafe1 == 1 ~ 5,
                               psycsafe1 == 2 ~ 4),
         psycsafe3 = case_when(psycsafe3 == 1 ~ 5,
                               psycsafe3 == 2 ~ 4),
         psycsafe5 = case_when(psycsafe5 == 1 ~ 5,
                               psycsafe5 == 2 ~ 4),
         teamsatisfaction2 = case_when(teamsatisfaction2 == 1 ~ 5,
                                       teamsatisfaction2 == 2 ~ 4))





