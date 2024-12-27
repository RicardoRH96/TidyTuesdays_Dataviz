# Tidy Tuesdays week 52 - Holiday Travel
## Author: Ricardo Rivero
### Source:
# Lai S., Sorichetta A. and WorldPop (2020). Global Public and School Holidays 2010-2019. 
# Mapping seasonal denominator dynamics in low- and middle-income settings, funded by The Bill and Melinda Gates Foundation.
# Lai S., Sorichetta A. and WorldPop (2020). Monthly volume of airline passengers in 90 countries 2010-2018. 
# Mapping seasonal denominator dynamics in low- and middle-income settings, funded by The Bill and Melinda Gates Foundation.

library(tidyverse)
library(sf)
library(gridExtra)
library(giscoR)
library(classInt)
library(ggtext)

# Data download and preparation
url_holidays <- "https://data.worldpop.org/GIS/Holiday_Data/public_holidays/public_holidays_2010_2019.zip"
path_holidays <- withr::local_tempfile(fileext = ".zip")
download.file(url_holidays, path_holidays)
global_holidays <- read_csv(path_holidays) |> 
  mutate(Date = lubridate::dmy(Date))

url_passengers <- "https://data.worldpop.org/GIS/Flight_Data/monthly_volume_of_airline_passengers/monthly_vol_of_airline_pass_in_90_countries_2010_2018.zip"
path_passengers <- withr::local_tempfile(fileext = ".zip")
download.file(url_passengers, path_passengers)
monthly_passengers <- read_csv(path_passengers) |>
  mutate(across(c(Year, Month), as.integer))

# Filter data for Christmas holidays and December flight data
holiday_list <- c('Christmas Eve', 'Christmas Day')

christmas_days <- global_holidays %>% 
  filter(Type == 'Public holiday') %>% 
  group_by(ADM_name, ISO3) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  summarize(n_holidays = n())

december_passengers <- monthly_passengers %>% 
  filter(Month == 12, Year == 2018) %>% 
  arrange(desc(Total)) %>% 
  slice(1:20)

# Choropleth map preparation
world <- giscoR::gisco_get_countries(year = 2016, epsg = "4326", resolution = 10) %>%
  rename(ISO3 = ISO3_CODE)

crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +datum=WGS84 +units=m +no_defs"

e_data <- merge(world, christmas_days, by = 'ISO3') %>%
  st_as_sf() %>%
  st_transform(crs = crsLAEA) %>%
  drop_na()

makeIntervals <- function(data, column) {
  data <- drop_na(data, {{ column }})
  breaks <- classIntervals(data[[as.character(substitute(column))]], n = 5, style = 'jenks')$brks
  labels <- paste0(round(breaks[-length(breaks)], 0), "–", round(breaks[-1], 0))
  data$cat <- cut(data[[as.character(substitute(column))]], breaks = breaks, labels = labels, include.lowest = TRUE)
  levels(data$cat) <- c(levels(data$cat), "No data")
  data$cat[is.na(data$cat)] <- "No data"
  return(data)
}

e_data <- makeIntervals(e_data, n_holidays)

christmas_palette <- c(
  "#384B70", "#507687", "#FFD700", "#B8001F", "#006400"
)

map <- ggplot() +
  geom_sf(data = e_data, aes(fill = cat), color = "white", size = 0.15, alpha = 0.9) +
  coord_sf(crs = 4326) +
  scale_fill_manual(values = christmas_palette) +
  labs(
    title = "Number of <span style='color:#1CB26B;'>Public</span> <span style='color:#D82428;'>Holidays</span> per Country",
    caption = "https://github.com/RicardoRH96, Dec 27, 2024, Tidy Tuesdays week 52"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "bottom",
    plot.title = element_markdown(size = 32, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "grey40", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Circular bar plot preparation
december_pass <- december_passengers %>%
  rename(ISO3 = ISO3) %>%
  left_join(e_data %>% select(ISO3, cat), by = "ISO3") %>%
  mutate(id = row_number())

label_data <- december_pass
number_of_bars <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bars
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

p <- ggplot(december_pass, aes(x = as.factor(id), y = sqrt(Total), fill = cat)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_polar() +
  geom_text(data = december_pass, aes(x = id, y = sqrt(Total) / 2, label = round(Total, 0), color = ifelse(ISO3 %in% c("CHN", "THA"), "black", "white")), fontface = "bold", angle = label_data$angle, show.legend = FALSE) +
  geom_text(data = label_data, aes(x = id, y = sqrt(Total) + 1, label = ISO3, hjust = hjust), color = "black", fontface = "bold", size = 3, angle = label_data$angle, inherit.aes = FALSE) +
  scale_fill_manual(values = christmas_palette) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.margin = unit(rep(-1, 4), "cm"),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Save outputs
ggsave('Downloads/circular_airTravelPlot.png', p, device = 'png', dpi = 1200)
ggsave('Downloads/HolidayMap.jpeg', map, device = 'jpeg', dpi = 1200, width = 18, height = 15, units = "in")