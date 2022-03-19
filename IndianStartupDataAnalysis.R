library(tidyverse)
library(ggplot2)
library(formattable)
library(gt)
library(dplyr)
library(stringr)
library(DT)
library(lubridate)

options(scipen = 999)
data <- read.csv("C:/Users/Prashant/Downloads/Indian Startup.csv")
head(data,n = 10)

data <- data %>%
  filter(Founded >= '2010' & Founded <= '2020')
data$Amount = str_sub(data$Amount,2,20)
data$Amount = gsub(",","",data$Amount,fixed = TRUE)
data$Amount[data$Amount == 'Undisclosed'] <- 0
data$Amount <- as.numeric(data$Amount)
data <- remove_missing(data)
data$Founded <- as.character((data$Founded),format = "%Y")


#---------------------------------------------------#


# Number of startups founded from 2010 to 2021
ggplot(data %>%
         filter(Headquarters %in% head(data %>%
                                       count(Headquarters,sort = 10),n = 10)$Headquarters[c(1:5)])) +
         geom_bar(mapping = aes(x = Founded,fill = Headquarters),position = 'dodge')
#-------------------------------------------------------#

#Top funded 10 sectors

datatable(data %>%
  select(Sector,Amount)%>%
  group_by(Sector)%>%
  summarise(tot_am = sum(Amount))%>%
  arrange(desc(tot_am))) %>% formatCurrency('tot_am',"$")

#---------------------------------------------------#

#How did different sectors got funded in the last 10 years?

ggplot(data %>%
         filter(Founded == 2016) %>%
         select(Sector,Amount,Founded) %>%
         group_by(Sector,Founded) %>%
         summarize(tot_amount=sum(Amount))%>%
         arrange(desc(tot_amount))%>%
         head(n = 5)) +
  geom_col(mapping = aes(x = Sector,y = tot_amount,fill = Sector))+
  labs(title = 'Top 5 sectors in 2016',x = 'Sector',y = 'Total Amount Funded')+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(data %>%
         filter(Founded == 2017) %>%
         select(Sector,Amount,Founded) %>%
         group_by(Sector,Founded) %>%
         summarize(tot_amount=sum(Amount))%>%
         arrange(desc(tot_amount))%>%
         head(n = 5)) +
  geom_col(mapping = aes(x = Sector,y = tot_amount,fill = Sector))+
  labs(title = 'Top 5 sectors in 2016',x = 'Sector',y = 'Total Amount Funded')+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(data %>%
         filter(Founded == 2018) %>%
         select(Sector,Amount,Founded) %>%
         group_by(Sector,Founded) %>%
         summarize(tot_amount=sum(Amount))%>%
         arrange(desc(tot_amount))%>%
         head(n = 5)) +
  geom_col(mapping = aes(x = Sector,y = tot_amount,fill = Sector))+
  labs(title = 'Top 5 sectors in 2016',x = 'Sector',y = 'Total Amount Funded')+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(data %>%
         filter(Founded == 2019) %>%
         select(Sector,Amount,Founded) %>%
         group_by(Sector,Founded) %>%
         summarize(tot_amount=sum(Amount))%>%
         arrange(desc(tot_amount))%>%
         head(n = 5)) +
  geom_col(mapping = aes(x = Sector,y = tot_amount,fill = Sector))+
  labs(title = 'Top 5 sectors in 2016',x = 'Sector',y = 'Total Amount Funded')+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

ggplot(data %>%
         filter(Founded == 2020) %>%
         select(Sector,Amount,Founded) %>%
         group_by(Sector,Founded) %>%
         summarize(tot_amount=sum(Amount))%>%
         arrange(desc(tot_amount))%>%
         head(n = 5)) +
  geom_col(mapping = aes(x = Sector,y = tot_amount,fill = Sector))+
  labs(title = 'Top 5 sectors in 2016',x = 'Sector',y = 'Total Amount Funded')+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

#---------------------------------------------------#

#Top 10 cities with startups

data %>%
  count(Headquarters,sort = TRUE) %>%
  head(n = 10) %>%
  gt(rowname_col = 'Headquarters') %>%
  tab_header(
    title = md('Top 10 cities with startups')
  ) %>%
  cols_label(n = 'Number of Startups') %>%
  cols_align(align = 'center')%>%
  cols_width(n ~ px(100))
#---------------------------------------------------#

c <- data %>%
  filter(Sector %in% c('EdTech','Automotive','Financial Services','E-commerce'))%>%
  select(Sector,Amount,Founded) %>%
  group_by(Sector,Founded)%>%
  summarize(total = sum(Amount))
ggplot(c)+
  geom_col(mapping = aes(x = Founded,y = total))+
  facet_wrap(~Sector,nrow = 2)

