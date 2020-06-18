# Libraries

library(tidyverse)
library(ggplot2)
library(scales)
library(extrafont)
library(dplyr)

# Data

census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')

# Filter

census <- census %>% 
  filter(region=='USA Total')

# Data Frame

df <- data.frame(
  Condition = rep(c("Free","Slave"), each = 9),
  Year = rep(c(census$year), 2),
  Cant = c(census$black_free,census$black_slaves))

# %

Años <- census %>% 
  select(year, black, black_free, black_slaves) 

Perc1 <- paste(round(Años$black_slaves/Años$black*100,2),"%")
Perc1[1]= " "
Perc2 <- paste(round(Años$black_free/Años$black*100,2),"%")
Porcentajes <- c(Perc1,Perc2)

#Plot

fill <- c("#8CD6BB","#F0884F")

graph<- ggplot(df, aes(x = Year, y = Cant))+
  geom_col(aes(fill = Condition), width = 8)+
  ggtitle("U.S Slavery (1790-1870)")+
  ylab("Black Population")+theme(axis.line = element_line(size=1, colour = "black"),
  panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
  panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, family="Tw Cen MT"),
  text=element_text(family="Verdana"), axis.title.y = element_text(vjust = 3), 
  axis.title.x = element_text(vjust = -2))

graph+scale_y_continuous(labels=comma)+scale_x_continuous(breaks = seq(from=min(census$year),to=max(census$year),by=20)) +
geom_text(aes(label =Porcentajes), size = 3, hjust = 0.4, vjust = -0.5, position = "stack")+scale_fill_manual(values=fill)


