#Tidytuesday week 12 (2022-03-22) - today we are looking at baby names

#load libraries
library(tidyr)
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)
library(ggtext)
library(showtext)
library(patchwork)
library(cowplot)
library(rcartocolor)
library(magick)
library(png)

#get data

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

babynames <- tuesdata$babynames

babymoji <- readPNG("babyfaces.png", native = TRUE)

#add fonts
font_add_google(name = "Lato", family= "lato")
font_add_google(name = "Bitter", family= "bitter")
showtext_auto()
#color sets

f_cols = c("#ff1c68", "#fa3477", "#ff528c",
           "#ff7aa7", "#fc8db3", "#fcb8d0")
m_cols = c("#0062ff", "#3885ff", "#5999ff",
           "#82b2ff", "#a6c8ff", "#bdd6ff")

#filter 5 of the most common latino/hispanic names for Male and Female

f_names = c("Maria", "Carolina", "Isabel", "Gabriela", "Sofia")
m_names = c("Carlos", "Jose", "Jorge", "Luis", "Juan")

count_female_names <- babynames %>% 
  filter(sex == "F", name %in% f_names) %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  slice_max(total, n = 5) %>% 
  mutate(rank = 1:5, 
         name = forcats::fct_reorder(name, -total)) %>% 
  pull(name)

lat_f_names <- babynames %>%
  filter(sex== "F", 
         name %in% f_names) %>%
  mutate(name = factor(name, levels = levels(count_female_names))) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n), 
            prop = sum(prop)) 

#same for male latino names
count_male_names <- babynames %>% 
  filter(sex == "M", name %in% m_names) %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  slice_max(total, n = 5) %>% 
  mutate(rank = 1:5, 
         name = forcats::fct_reorder(name, -total)) %>% 
  pull(name)

lat_m_names <- babynames %>%
  filter(sex== "M", 
         name %in% m_names) %>%
  mutate(name = factor(name, levels = levels(count_male_names))) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n), 
            prop = sum(prop)) 
#create one dataset for hispanic names

lat_names <- rbind(lat_f_names, lat_m_names)

#plot count of these hispanic names through time
num_f_names <- babynames %>%
  filter(sex=="F", name %in% f_names) %>%
  group_by(year, sex) %>%
  summarise(sum = sum(n))

#count hispanic male names
num_m_names <- babynames %>%
  filter(sex=="M", name %in% m_names) %>%
  group_by(year, sex) %>% 
  summarise(sum = sum(n))

p1 <- ggplot(data = NULL, 
             mapping = aes(x = year, y = sum, colour = sex)) + 
  geom_line(data = num_f_names, colour = "#ff1c68") +
  geom_point(data = num_f_names, size = 1) +
  geom_line(data = num_m_names, colour= "#0062ff") +
  geom_point(data = num_m_names, size = 1) +
  scale_colour_manual("", values = c(f_cols[1], m_cols[1]), labels = c("Female", "Male")) +
  scale_y_continuous(limits = c(0, 40000)) +
  coord_cartesian(expand = F) +
  labs(x = "", 
       y = "Number of Latino/Hispanic baby names") +
  theme(legend.position = c(0.1, 0.9), 
        legend.title = element_blank(), 
        legend.text = element_text(family = "lato", size = 14),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0), family = "lato"), 
        axis.text = element_text(family = "lato"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())

#plot most popular hispanic female names

p2 <- ggplot(data = lat_f_names, 
             mapping = aes(x = year, y = n)) + 
  geom_area(aes(group = name, fill = name), colour = f_cols[1]) +
  facet_wrap(~name, nrow = 1) +
  scale_y_continuous(limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(1900, 2017)) +
  scale_fill_manual(values = f_cols) +
  coord_cartesian(expand = F) +
  labs(x = "", y = "Babies per year\n") +
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), family = "lato"), 
        axis.text = element_text(family = "lato"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        strip.text = element_text(family = "bitter", size = 14),
        strip.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())

#plot male names

p3 <- ggplot(data = lat_m_names, 
             mapping = aes(x = year, y = n)) + 
  geom_area(aes(group = name, fill = name), colour = m_cols[1]) +
  facet_wrap(~name, nrow = 1) +
  scale_y_continuous(limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(1900, 2017)) +
  scale_fill_manual(values = m_cols) +
  coord_cartesian(expand = F) +
  labs(x = "", y = "Babies per year\n") +
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), family = "lato"), 
        axis.text = element_text(family = "lato"),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        strip.text = element_text(family = "bitter", size = 14),
        strip.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())

p3

#join plots

p <- p1 + (p2 / p3)  +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(title = "Tidy Tuesday week 12: Baby Names - Graph by @Ricardo__JRH", 
                  subtitle = "Trends in Latino/Hispanic baby names in the U.S through the years - Sofia is a rising star, Jose not that much...") &
  theme(plot.title = element_text(margin = margin(20, 0, 10, 0), family = "lato", size =32), 
        plot.subtitle = element_text(margin = margin(10, 0, 10, 0), family = "lato", size = 14),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff"), 
        plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"))


p

ggsave("tidytuesday12.jpg", plot = p, height = 25, width = 35, units = "cm", dpi = 900)
