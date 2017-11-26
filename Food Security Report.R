install.packages("DT")
install.packages("Knitr")
install.packages("plotly")
install.packages(("shiny"))                 
install.packages("flexdashboard")
install.packages("learningCurve")
install.packages("ggrepel")

library(data.table)
library(dplyr)
library(tibble)
library(DT)
library(knitr)
library(ggplot2)
library(plotly)
library(stringr)
library(tidyverse)
library(magrittr)
library(ggrepel)



url <- "https://raw.githubusercontent.com/nitishghosal/Food-Security-Indicators/master/Food_Security_Data_E_All_Data.csv"

raw_data <- as_data_frame(read.csv(url, stringsAsFactors = FALSE))

class(raw_data)

colnames(raw_data)

dim(raw_data)



## define a helper function
empty_as_na <- function(x){
  if ("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x) != "", x, NA)
}

## transform all columns
clean_data <- raw_data %>% mutate_each(funs(empty_as_na))


#clean_data <- as_data_frame(apply(raw_data, 2, function(x) gsub("^$|^ $", NA, x)))

clean_data <- raw_data %>% filter(Flag == "F" | Flag == "X") %>% separate(Year,into = c("Year_From","Year_To"))


clean_data$Area.Code <- as.character(clean_data$Area.Code)
clean_data$Item.Code <- factor(clean_data$Item.Code)
clean_data$Element.Code <- factor(clean_data$Element.Code)
clean_data$Year_From <-  as.numeric(clean_data$Year_From)
clean_data$Year_To <- as.numeric(clean_data$Year_To)

library(DT)
datatable(head(clean_data,50))



str(clean_data)

glimpse(clean_data)


#Create tidy data for GDP

GDP_tidy <- clean_data %>% filter(Item.Code == 22013 & Area.Code == 5000)
glimpse(GDP_tidy)

#Create tidy data for number of severely food insecure people(sfip)
sfip_tidy <- clean_data %>% filter(Item.Code == 21007)

#Create tidy data for number of people undernourished(pu)
pu_tidy <- clean_data %>% filter(Item.Code == 21001 & Year == "2014-2016")

#GDP trend for world from 2000-2016

GDP_tidy <- clean_data %>% filter(Item.Code == 22013)
glimpse(GDP_tidy)

plota <- GDP_tidy %>%
  filter(Area.Code == 5000) %>%
  ggplot(aes(Year_From,Value)) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Value)), linetype = "dashed", alpha = .5) +
  annotate("text", x = 2016, y = .8, label = "Average: 8.6%", size = 3)+
  scale_x_continuous(NULL, limits = c(2000, 2016.5), breaks = seq(2000, 2016, by = 2)) +
  scale_y_continuous(NULL, labels = scales::dollar,limits=c(10000, 16000), breaks = seq(10000,16000, by = 1000)) +
  ggtitle("Figure 1: World GDP value change from 2000-2016 ")+
  xlab("Year")+
  ylab("GDP Value in dollars")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia", size = 12),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))


ggplotly(plota)



p <- GDP_tidy %>%
  filter(Area.Code == 5000)%>%
  plot_ly(x = ~Year_From, y = ~Value, type = 'scatter', mode = 'lines+markers',
          hoverinfo = 'text',
          text = ~paste('GDP Value: ', Value,
                        '</br></br> Year: ', Year_From)) %>%
  layout(title = "Figure 1 : World GDP value changes over timer",
         xaxis = list(title = "Year"),
         yaxis = list(title = "GDP Vaue in USD"))
p

################################

  
  
#############################################################################  
# plotly map of 2014 savings by state
df <- GDP_tidy %>%
  filter(Area.Code != 5000, Year_From == 2016) %>%
  mutate(region = tolower(Area),
         GDP = (Value/1000),
         Code = Area.Code)

g <- list(
  scope = 'world',
  projection = list(type = 'albers world'),
  lakecolor = toRGB('white')
)

plot_geo(df, locationmode = 'country names') %>%
  add_trace(
    z = ~GDP, text = ~Area,
    locations = ~Area, colors = "Purples"
  ) %>%
  colorbar(title = 'GDP Value', len = .4, thickness = 20, xpad = 0, ypad = 0, x = 1.05, ticksuffix = 'k') %>%
  layout(
    title = paste(unique(df$Area), 'GDP Rate by Country<br>(Hover for breakdown)'),
    geo = g
  )  
  
###############################  
  
  
  
################################
#Find top and bottom 5 countries by GDP
GDP1K <- GDP_tidy %>%
  mutate(GDP=Value/1000)%>%
  filter(Area.Code != 5000)

top5 <- GDP1K %>%
  arrange(desc(GDP)) %>%
  filter(Year_From == 2016) %>%
  slice(1:5)

bottom5 <- GDP1K %>%
  arrange(GDP) %>%
  filter(Year_From == 2016) %>%
  slice(1:5)


avg <- GDP1K %>%
  group_by(Year_From) %>%
  summarise(Avg_mn = mean(GDP),
            Avg_md = median(GDP)) %>%
  mutate(Avg = "Average")

p1 <- top5 %>%
  plot_ly(x = ~Year_From, y = ~Value,color=~Area, type = 'bar')%>%
  layout(title = "Figure 1 : World GDP value changes over timer",
         xaxis = list(title = "Year"),
         yaxis = list(title = "GDP Vaue in USD"))
p1

ggplot(GDP1K, aes(Year_From, GDP, group = Area)) +
  geom_line(alpha = .1) +
  geom_line(data = filter(GDP1K,Area %in% top5$Area),
            aes(Year_From, GDP, group = Area), color = "dodgerblue") +
  geom_line(data = filter(GDP1K, Area%in% bottom5$Area),
            aes(Year_From, GDP, group = Area), color = "red") +
  geom_line(data = avg, aes(Year_From, Avg_mn, group = 1), linetype = "dashed") +
  annotate("text", x = 2016.25, y = .071, label = "Average", hjust = 0, size = 3) +
  geom_text_repel(data = top5, aes(label = Area), nudge_x = .5, size = 3) +
  geom_point(data = top5, aes(Year_From, GDP), color = "dodgerblue") +
  geom_text_repel(data = bottom5, aes(label = Area), nudge_x = 0.5, size = 3) +
  geom_point(data = bottom5, aes(Year_From, GDP), color = "red") +
  scale_x_continuous(NULL, limits = c(2000, 2016.25), breaks = seq(2000, 2016, by = 2)) +
  scale_y_continuous(NULL, limits = c(0,150), breaks = seq(0, 150, by = 15), labels = scales::dollar_format(prefix = "$", suffix = "K")) +
  ggtitle("Figure 4: GDP changes over time",
          subtitle = "Temporal assessment of Country-wise GDP change (2000-2016)") +
  xlab("Year")+
  ylab("GDP Value in dollars")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))

  
###########################
#Conflict

Stability_tidy <- clean_data %>% filter(Item.Code == 21032 & Flag == 'X')
glimpse(Stability_tidy)


top5 <- Stability_tidy %>%
  arrange(desc(Value)) %>%
  filter(Year == 2016) %>%
  slice(1:5)

bottom5 <- Stability_tidy %>%
  arrange(Value) %>%
  filter(Year == 2016) %>%
  slice(1:5)


avg <- Stability_tidy %>%
  group_by(Year) %>%
  summarise(Avg_mn = mean(Value),
            Avg_md = median(Value)) %>%
  mutate(Avg = "Average")

ggplot(Stability_tidy, aes(Year, Value, group = Area)) +
  geom_line(alpha = .1) +
  geom_line(data = filter(Stability_tidy,Area %in% top5$Area),
            aes(Year, Value, group = Area), color = "dodgerblue") +
  geom_line(data = filter(Stability_tidy, Area %in% bottom5$Area),
            aes(Year, Value, group = Area), color = "red") +
  geom_line(data = avg, aes(Year, Avg_mn, group = 1), linetype = "dashed") +
  annotate("text", x = 2016.25, y = .071, label = "Average", hjust = 0, size = 3) +
  geom_text_repel(data = top5, aes(label = Area), nudge_x = .5, size = 3) +
  geom_point(data = top5, aes(Year, Value), color = "dodgerblue") +
  geom_text_repel(data = bottom5, aes(label = Area), nudge_x = 0.5, size = 3) +
  geom_point(data = bottom5, aes(Year, Value), color = "red") +
  scale_x_continuous(NULL, limits = c(2000, 2016.25), breaks = seq(2000, 2016.25, by = 2)) +
  scale_y_continuous(NULL, limits = c(-3,2), breaks = seq(-3, 2, by = 0.2)) +
  ggtitle("Figure 4:  Political stability and absence of violence/terrorism (index)",
          subtitle = "Temporal assessment of Country-wise change (2000-2016)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))


##################################
#Number of People Undernourished


Undernourished <- clean_data %>% filter(Item.Code == 21001 & Area.Code == 5000)


a<-Undernourished %>%
  ggplot(aes(x=Year_To)) +
  geom_line(aes(y=Value)) +
  geom_hline(aes(yintercept = mean(Value)), linetype = "dashed", alpha = .5) +
  #annotate("text", x = 2010, y = .08, label = "Average: 8.6%", size = 3)+
  #scale_y_continuous(NULL, labels = scales::percent, limits = c(0, .115)) +
  #scale_x_continuous(NULL, breaks = seq(2000, 2014, by = 4)) +
  scale_x_continuous(NULL, limits = c(2001, 2016.5), breaks = seq(2001, 2016, by = 2)) +
  scale_y_continuous(NULL, limits = c(400,1000), breaks = seq(400,1000, by = 100)) +
  ggtitle("Figure 1: World Hunger on the Rise ",
          subtitle = "Prevalence and number of undernourished people in the world, 2000-2015. Figures for 2015 are projected estimates. SOURCE: FAO.") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia", size = 12),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))

ggplotly(a)

a <- Undernourished%>%
  plot_ly(x = ~Year_To, y = ~Value,type = 'scatter', mode = 'lines+markers',
          hoverinfo = 'text',
          text = ~paste('Number of undernourished people(Millions): ', Value,
                        '</br></br> Year: ', Year_To))%>%
  layout(title = "Figure 1 :Number of undernourished people in the world. SOURCE: FAO.",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of people undernourished (in Millions)"))
a


#Prevalence of undernourishment

PU <- clean_data %>% filter(Item.Code == 21004 & Area.Code == 5000)


b<-PU%>%
  ggplot(aes(x=Year,y=Value)) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Value)), linetype = "dashed", alpha = .5) +
  #annotate("text", x = 2010, y = .08, label = "Average: 8.6%", size = 3)+
  #scale_y_continuous(NULL, labels = scales::percent, limits = c(0, .165)) +
  #scale_x_continuous(NULL, breaks = seq(2000, 2014, by = 4)) +
  scale_x_continuous(NULL, limits = c(1999, 2015.5), breaks = seq(1999, 2015, by = 2)) +
  #scale_y_continuous(NULL, labels = scales::percent, limits = c(0, .165)) +
  ggtitle("Figure 1: World Hunger on the Rise ",
          subtitle = "Prevalence and number of undernourished people in the world, 1999-2015. Figures for 2015 are projected estimates. SOURCE: FAO.") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia", size = 12),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))

ggplotly(b)


b <- PU%>%
  plot_ly(x = ~Year_To, y = ~Value,type = 'scatter', mode = 'lines+markers',
          hoverinfo = 'text',
          text = ~paste('Prevalence of undernourishment (%) : ', Value,
                        '</br></br> Year: ', Year_To))%>%
  layout(title = "Figure 2 :Prevalence of undernourishment in the world. SOURCE: FAO.",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Prevalence of undernourishment (%)"))
b


p <- subplot(a, b, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE)
p
