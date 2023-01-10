#TidyTuesday 1, 2023. Bring your own data
#Also counts for #MapPromptMonday
#Analysis done by Ricardo Rivero H. @Ricardo__JRH, data collected from Colombia's INS, portal SIVIGILA

#In this analysis we are going to work with reported Dengue cases per year from 2007 to 2017, using 2007 as a baseline and visualizing the change in case numbers
#In the department of Cordoba, Colombia.

library(tidyverse)
library(data.table)
library(rgdal)
library(raster)
library(sf)
library(tmap)
library(gstat)
library(sp)
library(readxl)
library(terra)
library(showtext)
library(BAMMtools)
library(rstatix)
library(ggplot2)
library(scales)
library(cowplot)
library(colorspace)
library(extrafont)
library(extrafontdb)
library(showtext)
library(prettymapr)
library(mgcv)
library(ggformula)
library(stringi)
library(geodata)
library(gstat)
library(tidyterra)

dengue <- fread("~/Documents/R_data/denv_db.csv")

#Select only the variables that are important for analyzing the change in case numbers per year
dengue <- dengue %>%
  dplyr::select(ANO, SEMANA, EDAD, COD_DPTO_O, COD_MUN_O, Departamento, Municipio)


#We translate the column names to english and simplify them for easy manipulation
colnames(dengue) <- c('Year','Epiweek', 'Age', 'DepCode', 'MunCode', 'Dep','Mun')
dengue_1 <- dengue %>%
  group_by(Year, Mun) %>%
  summarise(Cases = n())



#to plot our data we will need to add the territory code (named DIVIPOLA in Colombia)
divipola <- read_xlsx("~/Documents/R_data/DIVIPOLA_municipios.xlsx")
colnames(divipola) <- c('Codigo','Depname','DIVIPOLA','Mun', 'Territory','Lat', 'Lon')

divipola <- divipola %>%
  dplyr::select(Codigo, Depname, DIVIPOLA, Mun, Lat, Lon) %>%
  mutate(names = stri_trans_general(str=Mun,
                                    id = "Latin-ASCII")) %>%
  dplyr::select(-Mun)
divipola <- divipola[-1,]

colnames(divipola) <- c('Codigo', 'Depname','MPIO_CDPMP','Lat','Lon', 'Mun')

#Add the DIVIPOLA code column to the Dengue database and filter the reports from unknown municipalities
coded_dengue <- merge(x = dengue_1, y = divipola, by = "Mun", all.x = TRUE) %>%
  drop_na()

#test cases data for normality and calculate mean/median
normality <- coded_dengue %>% group_by(Year) %>% shapiro_test(Cases) #data does not represent a normal distribution
nationalmedian <- median(coded_dengue$Cases)
#Load shp data from Colombia

#Plot number of cases for the 10 municipalities with higher count of cases

#find the municipalities with highest number of total cases
total <- coded_dengue %>% group_by(Mun) %>%
  summarise(Total = sum(Cases)) %>%
  arrange(desc(Total))

#top municipalities are 
highest <- c('CALI','MEDELLIN','BUCARAMANGA','IBAGUE','ARMENIA','VILLAVICENCIO','NEIVA','BARRANQUILLA','FLORIDABLANCA','PEREIRA','SINCELEJO','VALLEDUPAR')

base_text_size = 16

p1 <- coded_dengue %>% dplyr::filter(Cases > 10) %>%
  group_by(Mun) %>%
  mutate(Mean_cases = mean(Cases)) %>%
  ungroup() %>%
  dplyr::filter(Mun %in% highest) %>%
  group_by(Year) %>%
  ggplot(aes(x=Year, y=Cases, color=Mun))+
  geom_point(alpha=0.7) +
  geom_line(alpha=0.7)+
  scale_colour_viridis_d()+
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(breaks = c(2007, 2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) +
  theme_minimal(base_family = "Arial")+
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      face = "bold", size = base_text_size * 1.5,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = base_text_size,
      hjust = 0.5
    ),
    axis.text = element_text(size = base_text_size * 0.7, face = "italic"),
    legend.key = element_rect("transparent"),
    legend.position = "right",
    legend.title = element_text(size = base_text_size * .7),
    legend.text = element_text(size = base_text_size * .7),
    legend.spacing.x = unit(0, "pt")
  )


#Let's observe if there was a change in the age of infected people
p2 <- dengue %>% group_by(Year) %>%
  summarise(Mean_age = mean(Age), Cases= n()) %>%
  ggplot(aes(x=Year, y=Mean_age))+
  geom_point(aes(y= Mean_age, size=Cases), alpha=0.7)+
  geom_line(alpha=0.7) +
  geom_smooth(se=FALSE, color = "black", alpha=0.7) +
  scale_x_continuous(breaks = c(2007, 2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) +
  theme_minimal(base_family = "Arial")+
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      face = "bold", size = base_text_size * 1.5,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = base_text_size,
      hjust = 0.5
    ),
    axis.text = element_text(size = base_text_size * 0.7, face = "italic"),
    legend.key = element_rect("transparent"),
    legend.position = "right",
    legend.title = element_text(size = base_text_size * .7),
    legend.text = element_text(size = base_text_size * .7),
    legend.spacing.x = unit(0, "pt")
  ) +
  labs(y="Mean Age of cases", x="Year", caption = "Mean age of patients infected with DENV has been showing an increasing trend during the last years.")

p1_p2 <- ggpubr::ggarrange(ncol = 1, nrow = 2, p1, p2)



#Now let's map the incidence of Dengue Fever per 100k using Inverse Distance Weighted interpolation
#get population data and calculate incidence
population <- read_xlsx("~/Documents/R_data/poblacion.xlsx")
population <- population[-1,]
colnames(population) <- c('DepCode', 'Dep','MPIO_CDPMP','Mun','Year','Territory','Population')
population$Population <- as.numeric(population$Population)
population1 <- population %>% filter(Territory == "Total" & Year == 2018)

#We'll use only Dengue data for 2016 (highest case number)
map_for_dengue <- coded_dengue %>%
  dplyr::filter(Year == 2016) %>%
  dplyr::select(Cases, MPIO_CDPMP, Lat, Lon)
colnames(map_for_dengue) <- c('Cases_DENV', 'MPIO_CDPMP', 'Lat', 'Lon')


colombia_map <- st_read(dsn = "~/Documents/R_Data/shp",
                        layer = "MGN_MPIO_POLITICO") %>%
  st_transform(crs = 4326)

map_pop <- merge(x = colombia_map, y = population1, by = "MPIO_CDPMP", all.x = TRUE) %>%
  dplyr::select(-Dep,-Year, -Territory, -Mun, -DepCode)
map_data <- merge(x = map_pop, y = map_for_dengue, by = "MPIO_CDPMP", all.x = TRUE)

map_incidence <- map_data %>% mutate(Dengue_incidence = (Cases_DENV/Population)*100000)

#Extract only cases and long/lat for creating the matrix
municipios <- unique(map_incidence$MPIO_CDPMP)
divipola <- divipola %>% filter(MPIO_CDPMP %in% municipios)

df_dengue <- map_incidence %>% dplyr::select(Lon,
                                          Lat, Dengue_incidence)
df_dengue <- as.data.frame(df_dengue)
df_dengue <- df_dengue[,-4]
colnames(df_dengue) <- c('x','y','z')
df_dengue$x <- as.numeric(df_dengue$x)
df_dengue$y <- as.numeric(df_dengue$y)
df_dengue <- df_dengue[rowSums(is.na(df_dengue)) == 0, ] 
colombia_map1 <- as(map_incidence, "Spatial")
sp::coordinates(df_dengue) <- ~ x + y

grd <- as.data.frame(spsample(colombia_map1, "regular", n=1000000))
names(grd) <- c("x", "y") # rename the columns to x and y in grd
coordinates(grd) <- c("x", "y") # convert grd dataframe to SpatialPoints
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

proj4string(df_dengue) <- proj4string(df_dengue) # Temp fix until new proj env is adopted
proj4string(grd)     <- proj4string(df_dengue)

P.idw <- gstat::idw(z ~ 1, df_dengue, newdata = grd, idp = 2.0)

r       <- raster::raster(P.idw)
r.m     <- terra::mask(r, colombia_map1)

map.p <- raster::rasterToPoints(r.m)

df <- data.frame(map.p)

colnames(df) <- c("lon", "lat", "Dengue_incidence")
foo <- classInt::classIntervals(df$Dengue_incidence, n=10, style='fisher')
names(foo)
df$breaks <- df$Dengue_incidence
df$breaks <- cut(df$breaks, breaks = foo$brks, labels=as.character(1:10))

#cache download Colombia elevation data for mapping
mydir <- tempdir()
r_init <- elevation_30s("COL", path = mydir)


colombia <- st_read(dsn = "~/Documents/R_Data/shp",
                    layer = "MGN_MPIO_POLITICO") %>%
  st_transform(crs = 4326) %>% st_as_sf() %>% vect()
# For better handling we set here the names
names(r_init) <- "alt"

# We don't want values lower than 0 on the raster
r <- r_init %>%
  tidyterra::mutate(alt = pmax(0, alt)) #pmax returns parallel maxima of the input values

# Now intersect the raster and the vector and filter by range

exploded <- r %>%
  crop(colombia, mask = TRUE) %>% #cut out a part of a SpatRaster with a SpatExtent
  # Let's define here a range of elevations
  filter(alt > 0 & alt < 6000) %>%
  drop_na() %>%
  as.polygons(dissolve = TRUE, na.rm = TRUE) %>% #SpatRaster to polygon
  # Aggregate first
  terra::aggregate() %>% #aggregate to create a lower resolution SpatRaster
  # Explode vectors
  disagg() %>% #Dissolve SpatVector
  # And fill holes
  fillHoles() #remove holes from polygons

# Select biggest polygons (area bigger than 50 kms 2)
r_plain <- exploded %>%
  # Add area
  mutate(area = expanse(exploded)) %>% #expanse gets the area of individual polygons or for all raster cells
  filter(area > 50000**2) %>%
  # And convert to lines
  as.lines()

autoplot(r_plain)


# Creating hillshade

slope <- terrain(r, "slope", unit = "radians") #terra::terrain calculates the terrain characteristics from elevation data
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 45) #shade::terra computes hill shade from slope and aspect layers (must be in radians)
names(hill) <- "shades" #name normalization for simplicity

pal_greys <- hcl.colors(1000, "Grays") #Hillshading pallete

# Index of color by cell
index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>% #terra::rescale to rescale a SpatVector or SpatRaster, useful for inset maps
  mutate(index_col = round(index_col)) %>%
  pull(index_col) #extract a single column

# Get cols
vector_cols <- pal_greys[index]

#plot that s...
hill_plot <- ggplot() +
  geom_spatraster(
    data = hill, fill = vector_cols, maxcell = Inf,
    alpha = 1
  )

hill_plot


# Overlaying and theming

# Aware of limits of the raster

alt_limits <- minmax(r) %>% as.vector() #terra::minmax to compute the min and max cell values
# Round to lower and higher 500 integer with a min of 0
alt_limits <- pmax(
  c(floor(alt_limits[1] / 500), ceiling(alt_limits[2] / 500)) * 500,
  0
)

alt_limits

base_text_size <- 9

plot_col <- hill_plot +
  geom_spatraster(data = r, maxcell = Inf) +
  # Overlay the_plain
  geom_spatvector(
    data = r_plain,
    color = alpha("black", 0.7),
    linewidth = 0.15
  ) +
  scale_fill_hypso_tint_c(  #used for continous values
    palette = "wiki-schwarzwald-cont",
    limits = alt_limits,
    alpha = 0.4,
    breaks = seq(0, 3500, 250),
    labels = label_comma()
  ) +
  guides(fill = guide_legend(
    title = "   m.",
    title.position = "top",
    keywidth = .5,
    reverse = TRUE,
    override.aes = list(alpha = 0.8)
  )) +
  labs(
    title = "Zika vs Prec",
    subtitle = "Plotting of Average precipitation vs Zika Cases"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      face = "bold", size = base_text_size * 1.5,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = base_text_size * 0.9,
      hjust = 0.5
    ),
    plot.caption = element_text(
      margin = margin(t = base_text_size * 3),
      face = "italic"
    ),
    legend.key = element_rect("grey50"),
    legend.text = element_text(hjust = 0),
    legend.position = "left"
  )

plot_col

#now let's add the Dengue data

r.m_spat <- as(r.m, Class = "SpatRaster")
crs(r.m_spat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
compare_spatrasters(r.m_spat, hill)

#align the rasters
dengue_avg_mask <- r.m_spat %>%
  project(hill) %>%
  crop(hill) %>%
  mask(hill)

compare_spatrasters(r.m_spat, hill)

names(dengue_avg_mask) <- "dengue"
autoplot(dengue_avg_mask)

#create plot with overlaying rasters
custom_colors <- c('#5f4ea2','#3a7cb7','#4da5b1','#7acaa3','#b0dea2',
                            '#daf199','#f5fbb0','#fef3a9','#fcd681',
                            '#fdb365','#f6804c','#e55549','#ca304c',
                            '#9d1742', "#8d1755")
                            
zika_limits <- floor(as.vector(minmax(dengue_avg_mask)) / 100) * 100 + c(0, 100)

dengue_plot

p3 <- hill_plot +
  geom_spatraster(data = dengue_avg_mask, maxcell = Inf) +
  # Overlay the_plain
  geom_spatvector(
    data = r_plain, color = alpha("black", 0.7),
    linewidth = .1
  ) +
  # This part is theming only
  scale_fill_stepsn(
    colours = alpha(custom_colors, 0.7),
    na.value = NA,
    labels = label_comma(),
    breaks = foo$brks,
    values = scales::rescale(c(1.477602, 223.395530,
                               347.552919, 466.967310,
                               670.360804, 1045.451177,
                               1730.953186, 3284.690619,
                               7854.123562, 17153.315719,
                               26018.538880))) +  #seq(0, zika_limits[2], 50) 
  guides(fill = guide_legend(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    title.position = "right",
    label.position = "bottom",
    nrow = 1,
    family = "Arial",
    title = " /100k",
    override.aes = list(alpha = 0.9)
  )) +
  labs(
    title = "Incidence of Dengue infection during 2016",
    subtitle =  "Non-sampled Regions Predicted Using IDW Interpolation",
    caption = "Source: SIVIGILA, Instituto Nacional de Salud. Map by Ricardo Rivero H."
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(
      face = "bold", size = base_text_size * 1.5,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = base_text_size,
      hjust = 0.5
    ),
    axis.text = element_text(size = base_text_size * 0.7, face = "italic"),
    legend.key = element_rect("grey50"),
    legend.position = "bottom",
    legend.title = element_text(size = base_text_size * .7),
    legend.text = element_text(size = base_text_size * .7),
    legend.spacing.x = unit(0, "pt")
  )

p_final <- ggpubr::ggarrange(nrow=1, ncol=2, p3, p1_p2)
p_final

cowplot::ggsave2("~/Documents/R_data/TidyTuesday1_2023_DENV_hires.jpg", plot = p_final, dpi = 1500, width = 30, height = 16, units = "cm")
cowplot::ggsave2("~/Documents/R_data/DENV_MAP.jpg", plot = p3, dpi = 1500, width = 30, height = 16, units = "cm")
