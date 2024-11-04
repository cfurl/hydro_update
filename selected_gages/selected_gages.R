# need to show when generated

library(tidyverse)
library(sf)
library(dataRetrieval)
library(lubridate)
library(zoo)

# make your map
map <- read_sf(".\\gis\\usgs_basins.shp")

gages_map <- tibble (siteNo = c("08167500", "08195000", "08190000", "08178980", "08171290"), 
                     station.name=c("Guadalupe nr Spring Branch", "Frio at Concan", "Nueces at Laguna", "Medina nr Pipe Creek", "Blanco nr Kyle"),
                     x = c(-98.3836275, -99.7047756, -99.997287, -98.9793056, -97.9525 ),
                     y=c(29.8604957, 29.48856496, 29.42856679, 29.69438889, 30.00555556 ))%>% 
  st_as_sf( coords = c("x", "y"), crs=4269) 

p1 <- ggplot() +
  
  geom_sf(data = map, fill=NA) +
  geom_sf(data=gages_map)

# collect discharge functions

collect_discharge <- function (siteNo, start.date, end.date, station.name) {
  
  as_tibble(readNWISdv(siteNumbers = siteNo,
                       parameterCd = "00060", # this parameter code is daily mean discharge in cfs
                       startDate = start.date,
                       endDate = end.date)) |>
    select(2:4) |>
    mutate (site = station.name) |>
    purrr::set_names(c("usgs_no","date","cfs","station_name")) |>
    relocate (usgs_no, .after = last_col()) }



# Tibble for 4 gages

gages <- tibble (siteNo = c("08167500", "08195000", "08190000", "08178980", "08171290"), 
                 station.name=c("Guadalupe nr Spring Branch", "Frio at Concan", "Nueces at Laguna", "Medina nr Pipe Creek", "Blanco nr Kyle"))

start.date <- as.Date(Sys.time(),format="%Y-%m-%d") - 180
end.date <- as.Date(Sys.time(),format="%Y-%m-%d")



# initiate tibble for loop; bind_rows
# Loop throught collect discharge for 4 stations
flow_list <- tibble()

for (i in 1:length(gages$siteNo)) {
  
  siteNo <- gages[i,1] 
  start.date <- start.date
  end.date <- end.date
  station.name <- as.character (gages[i,2]) 
  
  
  flow_loop <- collect_discharge (siteNo, start.date, end.date, station.name)
  
  flow_list<- bind_rows(flow_list,flow_loop)
}

# Conduct the manual QA to identify missing values; input missing values from USGS or EAA Sharepoint  

flow_list |>
  arrange(desc(date)) |>
  print(n=60)

## Plot

theme_set(theme_bw())
theme_update(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),strip.text.x = element_text(size = 30))

ggplot(data = flow_list) +
  geom_line(mapping = aes(x = date, y = cfs), show.legend = FALSE)+
  facet_wrap(~ station_name, nrow = 2,scales="free")+
  labs(y=NULL,x=NULL)+
  xlim(as.Date(c(start.date,end.date)))

