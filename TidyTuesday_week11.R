#TidyTuesday March 15th, 2022 (week 11) - R package vignette counts!!

library(tidyr)
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)
library(ggtext)
library(showtext)

#download data
tuesdata <- tidytuesdayR::tt_load(2022, week = 11)
bioc <- tuesdata$bioc
cran <- tuesdata$cran
date_f <- "%Y-%m-%d"
pks <- c(
  'ggplot2','dplyr','tidyverse',
  'purrr','forcats','magrittr',
  'lubridate','tidytuesdayR',
  'tibble','stringr','readr')


#manipulate datasets for fixing date formats
#plot CRAN data


theme_set(
  theme_minimal() + 
    theme(legend.position = "upper right")
)

bioc$package <- as.factor(bioc$package)
bioc$rnw[bioc$rnw==0] <- NA
bioc$rmd[bioc$rmd==0] <- NA
bioc$newdate <- ymd(bioc$date)
data <- cran %>%
  filter(package %in% pks)) %>%
  mutate(date_s=as.Date(date, "%Y-%m-%d")) %>%
  gather(cran, key = "package") %>%
  group_by(rmd) %>%
  summarize(number = sum(value, na.rm = TRUE))

#format and manipulate CRAN
cran$package <- as.factor(cran$package)
cran$rmd[cran$rmd==0] <- NA
cran$newdate <- ymd(cran$date)
data_cran <- cran %>% drop_na() %>%
  filter(package %in% pks) %>%
  mutate(date_s=as.Date(date, "%Y-%m-%d")) %>%
  group_by(package) %>%
  mutate(crmd = cumsum(rmd)) 
  

#test this
data_cran2 <- data_cran %>%
  filter(package %in% pks) %>%
  mutate(date_s=as.Date(date, "%Y-%m-%d")) %>%
  group_by(package) %>% 
  mutate("Cummulative count" = cumsum(rmd)) %>%
  mutate(Package = package) %>%
  drop_na()

#label
year_lab <- as.Date(as.character(seq(2010,2022,2)),'%Y')

caption <- tibble(
  x=as.Date('2009','%Y'),
  y=265,
  lab="**Data:** R. Flight **| Plot:** @Ricardo__JRH ",
  color="#2b2e2e"
)



ggplot(data_cran2, aes(x = date_s, y = crmd, )) + 
  geom_point(aes(color = package, size = crmd), alpha = 0.7, na.rm=TRUE) +
  scale_color_viridis(discrete=TRUE, option = "turbo") +
  scale_size(range = c(0, 15)) +
  scale_x_date(
    date_labels = "%Y",
    limits=c(as.Date('2009','%Y'),as.Date('2023','%Y')),
    breaks=year_lab) +
  labs(x = "Version Release Date", y = "Number of Vignetes") +
  annotate(
    geom = 'text',x=as.Date('2009','%Y'),y=290,
    label="Vignette Literature of packages from the Tidyverse",size=7,
    color='#2b2e2e',
    family='Arial',
    hjust=0,vjust=1,lineheight=0.35
  )+
  annotate(
    geom = 'text',x=as.Date('2009','%Y'),y=275,
    label=
      "Each Point represents the cummulative number of vignettes per package version release date",
    size=4,
    family='Arial',color='#2b2e2e',
    hjust=0,vjust=1,lineheight=0.25
  ) +
  geom_richtext(data=caption, mapping=aes(x=x,y=y,label=lab),
    inherit.aes = FALSE,
    size=2.5, family='Arial',hjust=0,lineheight=0.45,vjust=1,
    color='#2b2e2e',
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")) +
    theme_minimal() + 
    theme(legend.position = "right",
            plot.margin = margin(1,1,1,1, unit = "cm"),
            plot.background = element_rect(fill="#b1bcbd",color=NA),
            axis.text.x = element_text(size=9,family='Arial',color='#2b2e2e'),
            axis.text.y=element_text(size = 9, family = "Arial", color="#2b2e2e"),
          axis.title.x = element_text(size = 14, family = "Arial", color = "#2b2e2e"),
          axis.title.y = element_text(size = 14, family = "Arial", color = "#2b2e2e"),
          legend.title = element_text(size = 10, family = "Arial", color = "#2b2e2e"),
          legend.text = element_text(family = "Arial", color = "#2b2e2e")
      ) +
  guides(color=guide_legend("Package")) +
  guides(size=guide_legend("Cummulative count"))
ggsave("TidyTuesday_week11.jpg", plot = last_plot(), height = 20, width = 30, units = "cm", dpi = 900)