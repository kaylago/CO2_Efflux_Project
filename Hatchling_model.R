
rm(list=ls())
library(ncdf4)
library(ncdf4.helpers)
library(dplyr)
library(readr)
library(tidyr)
library(PCICt)
require(sf)
library(raster)
library(tidyver)

climate <- paste0("C:/Users/kkmgo/Dropbox/CO2_Efflux/NOAAGlobalTemp_v5.nc")
climate_output <- nc_open(climate)
climate_output
climate_output$dim$time$units

climate2 = raster("C:/Users/kkmgo/Dropbox/CO2_Efflux/NOAAGlobalTemp_v5.nc",
             level = 180)
#climate_brick <- brick(climate)

#climate_brick

#plot(climate_brick[[1]])

#proj4string(climate2)=CRS("+init=EPSG:4326")

#climate_df = raster::as.data.frame(climate2, xy = TRUE)

#tas <- ncvar_get(climate_output, "tas")
#tas_time <- nc.get.time.series(climate_output, v = "tas",
                               #time.dim.name = "time")
#tas_time[c(1:3, length(tas_time) - 2:0)]

lon <- ncvar_get(climate_output, "lon")
lat <- ncvar_get(climate_output, "lat", verbose = F)
time <- ncvar_get(climate_output, "time")
temp <- ncvar_get(climate_output, "anom")

to = insol::JDymd(year = 1800, month = 1, day = 1)

# add the original time to the extracted time
jd = to+time

#convert the julian day to gregorian calender
date = insol::JD(jd, inverse = TRUE)

temp.df = NULL

for (i in 1:length(date)) {
  
  tempa = data.frame(lon,temp[,,i] %>% as.data.frame())%>% 
    as.tibble() %>% 
    gather(key = "key", value = "temp", 2:421) %>% 
    mutate(time = date[i], lat = rep(lat, each = 401)) %>% select(time, lon,lat, temp)
  
  temp.df = temp.df %>% bind_rows(tempa)
  
}

lon_index <- which.min(abs(lon - 85.3))
lat_index <- which.min(abs(lat - 29.8))
t_index <- 


# <- which.min(abs(lon - 36))
#lat_index <- which.min(abs(lat - 60))
time_index <- which(format(time, "%Y-%m-%d") == "2015-07-15")
tas[lon_index, lat_index, time_index]
