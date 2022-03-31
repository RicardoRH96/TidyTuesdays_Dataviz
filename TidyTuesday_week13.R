#TidyTuesday week 13 (2022-03-29) - Collegiate Sports Budgets (Second try)
#Data source: https://ope.ed.gov/athletics/#/datafile/list

#Load libraries
library(tidyr)
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggtext)
library(showtext)
library(patchwork)
library(cowplot)
library(sf)
library(grid)
library(gridExtra)
library(giscoR)
library(maps)
library(zipcodeR)
#Load datasets

data <- tt_load(2022, week = 13)

sports <- data$sports #look into which sports are practiced in the U.S
#add fonts
font_add_google(name = "Lato", family= "lato")
font_add_google(name = "Bitter", family= "bitter")
showtext_auto()

#filter database for obtaining only data on Football (the real football)

soccer <- sports[, c(1,2,3,4,5,6,7,10,11,12,22,23,24,25,26,27,28)] %>%
  filter(sports=="Soccer") %>%
  filter(year=="2019") %>%
  mutate(zipcode = zip_text)

soccer$ef_female_count[soccer$ef_female_count==0] <- NA
soccer$ef_male_count[soccer$ef_male_count==0] <- NA
soccer$ef_total_count[soccer$ef_total_count==0] <- NA

  
#create 2 dataset for classifying the 5 colleges with most students practicing football and plot revenues vs expenditures

f_cols = c("#ff1c68", "#fa3477", "#ff528c",
           "#ff7aa7", "#fc8db3")
m_cols = c("#0062ff", "#3885ff", "#5999ff",
           "#82b2ff", "#a6c8ff")
top_univ_count_fem = c("Pennsylvannia State University-Main Campus",
                   "University of Central Florida", 
                   "Ohio State University-Main Campus", 
                   "University of Connecticut",
                   "University of California-Davis")


top_univ_count_male = c("Pennsylvannia State University-Main Campus",
                        "Ohio State University-Main Campus",
                        "University of Central Florida",
                        "Michigan State University",
                        "Rutgers University-New Brunswick")


count_female_students <- soccer %>%
  drop_na() %>%
  filter(institution_name %in% top_univ_count_fem) %>%
  group_by(institution_name, ef_female_count) %>% 
  mutate(total = cumsum(ef_female_count)) %>%
  mutate(rev_thousand = rev_women/1000) %>%
  mutate(exp_thousand = exp_women/1000) %>%
  slice_max(total, n = 5) 



count_male_students <- soccer %>%
  drop_na() %>%
  filter(institution_name %in% top_univ_count_male) %>%
  group_by(institution_name, ef_male_count) %>%
  mutate(total = cumsum(ef_male_count)) %>%
  mutate(rev_thousand = rev_men/1000) %>%
  mutate(exp_thousand = exp_men/1000) %>%
  slice_max(total, n = 5)

#create a bubble plot over a map of the US for all football data

breaks <- c(1,10,100,1000,10000)

us_states <- map_data("state")
code <- geocode_zip(soccer$zipcode) %>%
  drop_na()
excluded_states = c("AL", "HI", "PR")
mapped_sports <- merge(x = soccer, y = code, by = "zipcode") %>%
  

final_dataset <- mapped_sports %>%
  arrange(ef_total_count) %>%
  mutate( institution_name=factor(institution_name, unique(institution_name))) %>%
  filter_all(all_vars(.!="AL")) %>%
  filter_all(all_vars(.!="HI")) %>%
  filter_all(all_vars(.!="PR"))

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))


pgray <- p + geom_polygon(color = "gray80", size = 0.1, fill = "grey", alpha = 0.5) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)+
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

pbubble <- pgray + geom_point(data=final_dataset, aes(x=lng, y=lat, size = ef_total_count, color = ef_total_count, alpha = ef_total_count), inherit.aes = FALSE, shape = 20, stroke=FALSE) +
  scale_size_continuous(name="Number of students", trans="log", range=c(1,15)) +
  scale_alpha_continuous(name="Number of students", trans="log", range=c(0.1, .9)) + 
  scale_color_viridis(trans="log", option = "viridis") +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"),
        plot.caption = element_text(size = 10, family = "lato", hjust = 0.5)) +
  labs(caption = "Number of students practicing Football per University (Contigous US States), greater point size indicates higher number of students.")

pbubble 



#create 2 individual plots for male and female comparing expenditures vs revenues in football

p2 <- ggplot(data = count_female_students,
             mapping = aes(x = year)) +
  geom_line(aes(y=rev_thousand), color = "#4cb33e") +
  geom_line(aes(y=exp_thousand), linetype="twodash", color="#e81c4f") +
  facet_wrap(~institution_name, nrow = 1) +
  scale_y_continuous(limits = c(0, 2000)) +
  labs(x="", y="Expenditures and Revenues per Thousand USD for Women") +
  theme(legend.position = "right",
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), family = "lato", size = 6), 
        axis.text = element_text(family = "lato", size = 4),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        strip.text = element_text(family = "bitter", size = 4, face = "bold"),
        strip.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())

p3 <- ggplot(data = count_male_students,
             mapping = aes(x = year)) +
  geom_line(aes(y=rev_thousand), color = "#4cb33e") +
  geom_line(aes(y=exp_thousand), linetype="twodash", color= "#e81c4f") +
  facet_wrap(~institution_name, nrow = 1) +
  scale_y_continuous(limits = c(0, 2000)) +
  labs(x="", y="Expenditures and Revenues per Thousand USD for Men", caption = "Revenues are represented by solid green lines while Expenditures are represented with red dashed lines.") +
  theme(legend.position = "right",
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        legend.background = element_rect(fill = "transparent", colour = "transparent"), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), family = "lato", size = 6), 
        axis.text = element_text(family = "lato", size = 4),
        plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"), 
        strip.text = element_text(family = "bitter", size = 4, face = "bold"),
        strip.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
        panel.grid.major = element_line(colour = "#DEDEDE"), 
        panel.grid.minor = element_blank())

p3

#join plots

p <- pbubble + (p2 / p3)  +
  plot_layout(widths = c(3, 2)) +
  plot_annotation(title = "Tidy Tuesday week 13: Collegiate Sports Budgets - Graph by @Ricardo__JRH", 
                  subtitle = "Althought Football is not the most popular sport in the US, data shows a higher college student participation mostly in West and East coast states.") &
  theme(plot.title = element_text(margin = margin(20, 0, 10, 0), family = "lato", size =24), 
        plot.subtitle = element_text(margin = margin(10, 0, 10, 0), family = "lato", size = 12),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff"), 
        plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"))

p


ggsave("tidytuesday13.jpg", plot = p, height = 20, width = 40, units = "cm", dpi = 900)
