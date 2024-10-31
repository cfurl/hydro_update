# Hydrologic update figure 3; San Marcos and Comal Springs

# Description: This figure is comprised of 4 hydrographs (3 Comal + 1 San Marcos)
# and some simple cards.

library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(zoo)


# Collect discharge function

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

gages <- tibble (siteNo = c("08170500", "08168710", "08168913", "08168932"), 
                 start.date = c("2023-10-28","2023-10-28", "2023-10-28", "2023-10-28"),
                 end.date=c("2024-10-28","2024-10-28", "2024-10-28", "2024-10-28"),
                 station.name=c("San Marcos", "Comal", "Old Channel Comal", "New Channel Comal"))

# initiate tibble for loop; bind_rows
# Loop throught collect discharge for 4 stations
flow_list <- tibble()

for (i in 1:length(gages$siteNo)) {
  
  siteNo <- gages[i,1] 
  start.date <- gages[i,2] 
  end.date <- gages[i,3] 
  station.name <- as.character (gages[i,4]) 
  
  flow_loop <- collect_discharge (siteNo, start.date, end.date, station.name)
  
  flow_list<- bind_rows(flow_list,flow_loop)
}

# Conduct the manual QA to identify missing values; input missing values from USGS or EAA Sharepoint  

flow_list |>
  arrange(desc(date)) |>
  print(n=20)

comal_manual <- tibble (date = as.Date(c("2024-10-26", "2024-10-27", "2024-10-28")),
                        cfs = c(57, 60, 60),
                        station_name = c("Comal", "Comal", "Comal"),
                        usgs_no = c("08168710", "08168710", "08168710"))

#san_marcos_manual <- tibble (date = as.Date(c("2024-10-26", "2024-10-27", "2024-10-28")),
#                        cfs = c(57, 60, 60),
#                        station_name = c("San Marcos", "San Marcos", "San Marcos"),
#                        usgs_no = c("08170500", "08170500", "08170500"))


# bind up your dataRetrieval and manual inputs

flow_list <- bind_rows (flow_list, comal_manual)

# Set up data for plotting and include ribbons

ribbon <- read_csv(".//comal_san_marcos//comal_san_marcos_ribbon_data.csv")

dat_4_plot <- flow_list |>
  mutate(month = month(date), day = day(date)) |>
  left_join(ribbon, by = c("month" = "month", "day" = "day", "station_name" = "station_name"))

# Metrics for cards

cards <- flow_list |>
  filter (station_name == "Comal" | station_name == "San Marcos") |>
  arrange(date) %>%
  group_by(station_name) |>
  mutate (thirty_day_avg = rollmean(cfs,30,align="right",fill=NA)) |>
  slice_max (date, n=1) |>
  ungroup ()

# Include area where you can place a message if you would like.  Graph should work w or wo msg.

## Plot

theme_set(theme_bw())
theme_update(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20),strip.text.x = element_text(size = 30))

ggplot(data = dat_4_plot) +
  geom_line(mapping = aes(x = date, y = cfs), show.legend = FALSE)+
  geom_ribbon(aes(x = date, ymin=perc25,ymax=perc75))+
  facet_wrap(~ station_name, nrow = 2,scales="free")+
  labs(y=NULL,x=NULL)+
  xlim(as.Date(as.character(c(gages[1,2],gages[1,3]))))












