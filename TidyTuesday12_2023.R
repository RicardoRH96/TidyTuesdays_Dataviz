#TidyTuesday week 12 = Programming Languages

library(tidyverse)
library(ggtext)
library(ggview)
library(cowplot)
library(magick)

#Load data

tuesdata <- tidytuesdayR::tt_load(2023, week = 12)
languages <- tuesdata$languages

#Explore data
head(languages)
# A tibble: 6 × 49
# pldb_id title descr…¹ type  appea…² creat…³ website domai…⁴ domai…⁵ refer…⁶ isbndb book_…⁷ seman…⁸ langu…⁹ githu…˟ githu…˟ githu…˟ githu…˟ githu…˟ githu…˟ githu…˟ githu…˟ githu…˟ githu…˟
# <chr>   <chr> <chr>   <chr>   <dbl> <chr>   <chr>   <chr>     <dbl> <chr>    <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>   <dbl> <chr>  
#   1 java    Java  NA      pl       1995 James … https:… NA           NA NA         400     401      37       0 NA           NA      NA      NA      NA      NA NA           NA      NA Java   
# 2 javasc… Java… NA      pl       1995 Brenda… NA      NA           NA https:…    349     351      48       1 NA           NA      NA      NA      NA      NA NA           NA      NA JavaSc…
# 3 c       C     NA      pl       1972 Dennis… NA      NA           NA http:/…     78      78      19       2 NA           NA      NA      NA      NA      NA NA           NA      NA C      
# 4 python  Pyth… NA      pl       1991 Guido … https:… python…    1995 https:…    339     342      52       3 NA           NA      NA      NA      NA      NA NA           NA      NA Python 
# 5 sql     SQL   NA      quer…    1974 Donald… NA      NA           NA NA         177     182      37       4 NA           NA      NA      NA      NA      NA NA           NA      NA SQL    
# 6 cpp     C++   NA      pl       1985 Bjarne… http:/… isocpp…    2012 NA         128     128       6       6 NA           NA      NA      NA      NA      NA NA           NA      NA C++    


#Explore the data to see the number of users for each language

languages <- languages %>% arrange(desc(number_of_users)) %>%
  filter(type=="pl")

#We take the 20 most popular languages for the analysis
most_pop <- languages[1:10,] %>%
  select(title, type, github_language_repos, number_of_users, number_of_jobs) %>%
  mutate(user_per_job = number_of_users/number_of_jobs) %>%
  mutate(title = case_when(title=='Arduino Programming Language'~'Arduino', TRUE ~ title))

#create color dictionary
title <- most_pop$title
color <- c("#f7df1e", "#f89820", "#00549D", "#283593", "#0277BD", "#be2909", "#8993be", "#165CAA", "#006699", "#00979C")
for_plot <- data.frame(title, color) %>%
  mutate(title = case_when(title=='Arduino Programming Language'~'Arduino', TRUE ~ title))
most_pop <- left_join(most_pop, for_plot, by = "title")
colnames(most_pop) <- c("title", "type", "github_language_repos","number_of_users","number_of_jobs","user_per_job","logo_color")

#Create labels for the columns of the plot

labels = paste0("<span style='font-family:Ubuntu;font-size:48pt;'><b>", most_pop$title, "</b><br></span>",
                "<span style='font-family:Ubuntu;font-size:36pt;'>", scales::comma(most_pop$user_per_job), "</span>")

#add custom fonts
sysfonts::font_add_google("Ubuntu","ubuntu")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)



#read images for logos
r_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/R.png")
python_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/python.png")
c_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/C.png")
java_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/java.png")
cplus_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/cplus.png")
php_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/php.png")
mat_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/matlab.png")
js_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/js.png")
spss_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/spps.png")
ard_logo <- image_read("~/Documents/R_Data/TidyTuesday/logos/arduino.png")

p <- ggplot(most_pop, aes(y=reorder(title, user_per_job), x=user_per_job, fill=I(logo_color)))+
  geom_col(alpha=0.7) +
  geom_richtext(aes(y=reorder(title, user_per_job), x=user_per_job, label=labels, color="black", fill="black"),
                hjust=0.6, fill=NA, label.colour=NA, lineheight=.9, text.color="black") +
  xlab("Users per Job") +
  ylab("Programming Language")+
  labs(title = "From the 10 most popular programming languages, R is currently the 5th with most job openings per user", 
       subtitle = "Python is the programming language with most job openings per active users, followed by C and Java, on the other hand, languages like SPSS and Arduino seem to be in the way of being completely forgiven.",
       caption = "Source: Programming Language DataBase, Visualization by Ricardo Rivero H. @RicardoRH_IDB")+
  scale_x_continuous(trans = "log10")+
  theme_minimal()+
  theme(plot.subtitle = element_textbox(family = "Ubuntu", size = 52, halign=0.4),
        plot.title = element_textbox(family = "Ubuntu", size = 64, halign = 0.5),
        plot.caption = element_textbox(family = "Ubuntu", size = 36, hjust = 0.9),
        legend.position = "none",
        plot.margin = margin(t=20, r=25),
        axis.text = element_blank())
final_p <- ggdraw()+
  draw_plot(p, x = 0, y = 0.15, width = 1, height = 0.85)+
  draw_image(python_logo,x = 0.65, y = 0.20, width = 0.05, height = 0.05)+
  draw_image(c_logo,x = 0.66, y = 0.28, width = 0.05, height = 0.05)+
  draw_image(java_logo,x = 0.67, y = 0.36, width = 0.05, height = 0.05)+
  draw_image(cplus_logo,x = 0.69, y = 0.427, width = 0.05, height = 0.05)+
  draw_image(r_logo,x = 0.70, y = 0.50, width = 0.05, height = 0.05)+
  draw_image(php_logo,x = 0.71, y = 0.56, width = 0.07, height = 0.07)+
  draw_image(mat_logo,x = 0.73, y = 0.64, width = 0.05, height = 0.05)+
  draw_image(js_logo,x = 0.75, y = 0.717, width = 0.05, height = 0.05)+
  draw_image(spss_logo,x = 0.77, y = 0.789, width = 0.05, height = 0.05)+
  draw_image(ard_logo,x = 0.95, y = 0.865, width = 0.05, height = 0.05)

ggsave2(filename ="~/Documents/R_Data/TidyTuesday/TidyTuesday12.jpg" ,final_p, dpi = 1200)

