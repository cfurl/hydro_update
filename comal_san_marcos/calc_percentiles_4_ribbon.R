#libraries
library(dataRetrieval)
library(tidyverse) # squeeze this down if you build a docker container
library(lubridate)
library(gridExtra)

# collect_discharge function. 
# this function returns mean daily usgs discharge. Use complete years only if you
# plan to feed this to 'calculate_percentile_annual_discharge()'
# data from this function are used in annual hydrograph line graphs (large panels)

collect_discharge_75_25 <- function (siteNo, start.date, end.date, station.name) {
  
  as_tibble(readNWISdv(siteNumbers = siteNo,
                       parameterCd = "00060", # this parameter code is daily mean discharge in cfs
                       startDate = start.date,
                       endDate = end.date)) |>
    select(2:4) |>
    set_names(c("usgs_no","date","cfs")) |>
    relocate (usgs_no, .after = last_col()) |>
    mutate (site = station.name)
}

# calculate_percentile_annual_discharge () function. (use complete years only)
# this function uses collect_discharge() as input and
# returns a percentile of mean annual flow from 1:100 using quantile() method.
# data from this function create the line graphs in the percentile
# plots, small graphs on right

calculate_percentile_75_25 <- function (collect_discharge_output) {
  
  mean_annual_flow <- collect_discharge_output |>
    mutate(year = year(date), month = month(date), day = day(date)) |>
    group_by(month, day)|>
    summarise(perc05 = quantile(cfs, 0.05),
              perc10 = quantile(cfs, 0.10),
              perc25 = quantile(cfs, 0.25), 
              median = median(cfs), 
              perc75 = quantile(cfs, 0.75),
              perc90 = quantile(cfs, 0.90), 
              perc95 = quantile(cfs, 0.95)) 
  
}

# Create data for Comal line graphs and current year percentile point
# Comal springflow period of record with full historical calendar years: 1928 - 2023

comal_por <- collect_discharge_75_25("08168710","1928-01-01","2024-10-30","Comal")
comal_percentile_75_25 <- calculate_percentile_75_25 (comal_por) |>
  mutate (station_name = "Comal")

# Create data for San Marcos line graphs and current year percentile point
# San Marcos springflow of record with full historical calendar years: 1957 - 2023
san_marcos_por <- collect_discharge_75_25("08170000","1957-01-01","2024-10-30","San Marcos")
san_marcos_percentile_75_25 <- calculate_percentile_75_25 (san_marcos_por) |>
  mutate (station_name = "San Marcos")

aa <- bind_rows(comal_percentile_75_25, san_marcos_percentile_75_25)
write_csv (aa, ".//comal_san_marcos//comal_san_marcos_ribbon_data.csv")



