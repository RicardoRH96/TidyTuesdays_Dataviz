#TidyTuesday 2023, week 14

#Load libraries
library(tidyverse)
library(rsvg)
library(ggimage)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)
library(magick)
library(cowplot)


#Load Data
data <- tidytuesdayR::tt_load('2023-04-04')
#It's called "football" not "SoCCeR"
football <- data$`soccer21-22`

sysfonts::font_add_google("Oswald","Oswald")
sysfonts::font_add("premier","~/Downloads/premier.ttf")
showtext_auto()
showtext_opts(dpi=600)


#Idea 1: Beeswarm plot of total goals scored
teams <-  sort(unique(football$HomeTeam)) #Create list of Premier League teams
links <- c('https://upload.wikimedia.org/wikipedia/en/5/53/Arsenal_FC.svg' , 
           'https://upload.wikimedia.org/wikipedia/en/f/f9/Aston_Villa_FC_crest_%282016%29.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/2/2a/Brentford_FC_crest.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/f/fd/Brighton_%26_Hove_Albion_logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/6/62/Burnley_F.C._Logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/c/cc/Chelsea_FC.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/a/a2/Crystal_Palace_FC_logo_%282022%29.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/7/7c/Everton_FC_logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/5/54/Leeds_United_F.C._logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/2/2d/Leicester_City_crest.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/0/0c/Liverpool_FC.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/e/eb/Manchester_City_FC_badge.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/7/7a/Manchester_United_FC_crest.svg' , 
           'https://upload.wikimedia.org/wikipedia/en/5/56/Newcastle_United_Logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/1/17/Norwich_City_FC_logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/c/c9/FC_Southampton.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/b/b4/Tottenham_Hotspur.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/e/e2/Watford.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/c/c2/West_Ham_United_FC_logo.svg' ,
           'https://upload.wikimedia.org/wikipedia/en/f/fc/Wolverhampton_Wanderers.svg') #Taken from https://github.com/natrivera/tidytuesday/blob/main/2023/2023-04-04/tt_2023-04-04.Rmd THANKS NAT!

team_links <- data.frame(teams, links)

#create a new dataframe comprising the total number of goals per team, one dataframe for away goals and one for home goals
teams_1 <- football %>%
  select(HomeTeam , FTHG) %>%
  group_by(HomeTeam) %>%
  summarise(home_goals = sum(FTHG))

colnames(teams_1) <- c('Team','FTHG')

teams_2 <- football %>%
  select(AwayTeam, FTAG) %>%
  group_by(AwayTeam) %>%
  summarise(away_goals = sum(FTAG))
colnames(teams_2) <- c('Team','FTAG')

teams <- left_join(teams_1, teams_2, by = "Team") %>%
  rowwise() %>%
  mutate(total_g = FTAG + FTHG) %>%
  left_join(team_links, by=c('Team'='teams'))


#Jitter plot

ggplot(teams, aes(x=total_g, y=Team))+
  geom_jitter() +
  geom_image(aes(y=Team, x=total_g, image=links), size=0.08, asp=9.5/6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90 , hjust = 0),
    axis.title.y = element_blank(),
    plot.title = element_markdown(size=14 , face='bold' , color='#0D2149'),
    plot.subtitle =  element_markdown(size=8),
    plot.caption = element_markdown()
  ) 

#Beeswarm try (Thanks to Tanya Shapiro)

df_plot = teams%>%
  group_by(total_g)%>%
  mutate(group_count=n(),
         row = row_number()-1,
         type = case_when(group_count %% 2 ==0 ~ "even", TRUE ~ "odd"),
         spacer = case_when(total_g>80 ~ 1.5, 
                            total_g>=80 ~ 1.25, 
                            group_count<13 ~ 0.9, 
                            TRUE ~ 0.5),
         max =0-((group_count/2)-0.5)*spacer,
         pos = max + spacer*row)

#Title and labs
title = "<span style='font-family:Oswald;color:white;font-size:35pt;'><span style=color:#6CABDD>**Attack</span> wins you games... and titles too! **</span>"
subtitle = "<p>Manchester City and Liverpool were the 2021-2022 Premier League's Top scoring teams, but the title was decided by only 5 goals.</p>"
caption = paste0("<span>**Source: https://www.kaggle.com/datasets/evangower/premier-league-match-data**</span>")

set.seed(960829)
p <- ggplot()+
  #change up size for images in different total_g ranges
  #size cannot be mapped to aes, set manually outside of aes
  geom_image(data=df_plot|>filter(total_g==99),
             aes(y = total_g, x = pos, image=links),
             position = position_jitter(width=1),
             size = 0.08,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(total_g>=52 & total_g<99),
             aes(y = total_g, x = pos, image=links),
             position = position_jitter(width=0.7),
             size = 0.08,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(total_g>=42 & total_g<52),
             aes(y = total_g, x = pos, image=links),
             position = position_jitter(width = 0.5),
             size = 0.08,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(total_g<42),
             aes(y = total_g, x = pos, image=links),
             position = position_jitter(width=0.5),
             size = 0.08,
             asp=9.5/6) +
  coord_flip() +
  scale_x_continuous(limits=c(-5.8,5.8), expand=c(0,0))+
  labs(title = "<span style='font-family:premier;color:white;font-size:52pt;'><span style=color:#6CABDD>**Attack**</span> wins you games. and <span style=color:#FFB404>titles</span> too.</span>",
       subtitle = "<p>Manchester City and Liverpool were the 2021-2022 Premier League's Top scoring teams, but the <span style=color:#FFB404>title</span> was decided by only <span style=color:#6CABDD>5</span> goals.</p>",
       caption = paste0("<span>**Source:** Premier League Match Data 2021-2022 via Evan Gower on Kaggle.<br> DataViz by <span style=color:black>**@RicardoRH_IDB**</span><span>"))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="#567d46", color="#567d46"),
        plot.title = element_textbox(),
        plot.subtitle  = element_textbox(color="#D7DDDD", size=26, margin=margin(t=8, b=10)),
        plot.caption = element_textbox(color="#D7DDDD", margin=margin(t=15), size=24),
        plot.margin = margin(t=20, r=20, l=20, b=10),
        text = element_text(color="white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="white",linewidth=0.25),
        axis.text.y=element_blank(),
        axis.text.x=element_text(family="Oswald", color="white", size=16),
        axis.ticks = element_blank(),
        axis.title = element_blank())

  factor = 19/14

  ggsave("~/Documents/R_Data/TidyTuesday/TidyTuesday14.jpg", dpi = 1200)
  
  
tidy <- magick::image_read("~/Documents/R_Data/TidyTuesday/TidyTuesday14.jpg")
crown <- magick::image_read("~/Documents/R_Data/TidyTuesday/crown.png")


final_p <- ggdraw() +
  draw_plot(p) +
  draw_image(crown, x = 0.835, y = 0.42, width = 0.2, height = 0.3)
final_p

ggsave("~/Documents/R_Data/TidyTuesday/TidyTuesday14_2.png",plot = final_p, dpi = 1200)
ggsave("~/Documents/R_Data/TidyTuesday/TidyTuesday14_2.jpg",plot = final_p, dpi = 1200)



