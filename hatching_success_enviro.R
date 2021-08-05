
rm(list=ls())
library(ncdf4)
library(ncdf4.helpers)
library(dplyr)
library(readr)
library(tidyr)
library(PCICt)
require(sf)
library(raster)
library(tidyverse)
library(AWAPer, warn.conflicts = FALSE)
library(readr)

climate <- paste0("C:/Users/kkmgo/Dropbox/CO2_Efflux/HATCHING_SUCCESS_MODEL/NOAAGlobalTemp_v5.nc")
climate_output <- nc_open(climate)
summary(climate_output)
climate_output$dim$time$units
print(climate_output)

lon <- ncvar_get(climate_output, "lon")
lat <- ncvar_get(climate_output, "lat")
time <- ncvar_get(climate_output, "time")
temp <- ncvar_get(climate_output, "anom")

dim(lon)
dim(lat)
dim(time)
dim(temp)



#time <- as.POSIXct(time,origin="1800-01-01 00:00:00")
#time

lat <= 30  #spot 24-25
lat >=29
lon <=86
lon >=85 #18-19

#time <= ("2015-01-01 00:00:00 LMT")

#time2 <- time[1600:1698]

#time2 <= ("2012-01-01 00:00:00 LMT")
#time2

to = insol::JDymd(year = 1800, month = 1, day = 1)

# add the original time to the extracted time
jd = to+time

#convert the julian day to gregorian calender
date = insol::JD(jd, inverse = TRUE)
date

lat2<-lat[24:24]
lat2

lon2 <-lon[18:18]
lon2

date2 <- date[1560:1624]
date2 ## works!!!!

date= as.data.frame(date2)

temp2 <- temp[18:18,24:24,1560:1624]
temp2

temp=as.data.frame(temp2)

df <- cbind(date,temp)

library(dplyr)

df_tempanom <- df %>% mutate(lon=rep(-87.5)) %>% mutate(lat=rep(27.5))

nc_close(climate_output)

clim_data<- read.csv("C:/Users/kkmgo/Dropbox/CO2_Efflux/HATCHING_SUCCESS_MODEL/Climate_Data.txt",header=TRUE)

clim_data2 <- clim_data %>% group_by(YEAR,MONTH) %>% summarize(mean.precip = mean(PRECIPITATION),mean.max.temp=mean(MAX.TEMP),mean.min.temp=mean(MIN.TEMP),mean.temp=mean(MEAN.TEMP))

names(clim_data2) <- c("year","month","mean.precip","mean.max.temp","mean.min.temp","mean.temp")

#month <- c(2,3,4,5,6,7,8,9,10,11,12,1,2,3)
#year <- c(2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2015,2015,2015)
#month=as.data.frame(month)
#year=as.data.frame(year)

#df_tempanom <- cbind(df_tempanom,year)
#df_tempanom <- cbind(df_tempanom,month)

system.time(df_tempanom$date2 <-
              as.POSIXct(
                strftime(df_tempanom$date2, format = "%Y-%m-%d %H:%M:%S"),
                format = "%Y-%m-%d",
                tz = "EST"
              ))

library(tidyverse)
library(lubridate)


df_tempanom = df_tempanom %>% 
  mutate(date2 = ymd(date2)) %>% 
  mutate_at(vars(date2), funs(year, month, day))


clim_data3 <- merge(clim_data2,df_tempanom,by=c("year","month"))

library(plyr)
setwd("C:/Users/kkmgo/Dropbox/CO2_Efflux/HATCHING_SUCCESS_MODEL/TIDEPREDS/")
tidedata_1 = list.files(pattern="*.txt",full.name=TRUE)
tide_data = ldply(tidedata_1, read.table, skip=12,header= T,row.names=NULL)

names(tide_data) <- c("date","weekday","time","time.per","tide.pred","tide.type")

tide_data$date <- as.Date(tide_data$date,format = "%Y/%m/%d")

tide_data = tide_data %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

detach(package:plyr)
tide_data_sum <- tide_data %>% group_by(year,month,tide.type) %>% summarize(mean.tide.pred=mean(tide.pred))

clim_data3 <- clim_data3 %>% filter(month>4) %>% filter(month<10)

tide_data_sum <- tide_data_sum %>% filter(month>4) %>% filter(month<10)

tide_data_sum <- tide_data_sum %>% spread()


hatch14 <- read.csv("C:/Users/kkmgo/Dropbox/CO2_Efflux/HatchingData2014_AllBeaches_AllZones.csv",header=TRUE)

library(tidyr)
library(dplyr)
hatch14 <- hatch14 %>% mutate(zone= ifelse(section=="seaward","littoral",ifelse(section=="mid","berm","dunes")))


hatch14<- hatch14 %>%
  mutate(laid = na_if(laid, "N/A"))  %>%
  mutate(emerged = na_if(emerged, "N/A"))  %>%
  mutate(hatched = na_if(hatched, "N/A"))  %>%
  mutate(success = na_if(success, "N/A"))

hatch14$laid <- as.numeric(hatch14$laid)
hatch14$emerged <- as.numeric(hatch14$emerged)
hatch14$hatched <- as.numeric(hatch14$hatched)

#hatch14 <- hatch14 %>% mutate(failed.h=laid-hatched) %>% mutate(failed.e=laid-emerged)

hatch14 <- hatch14 %>% mutate(emerge.success=emerged/laid*100,hatch.success=hatched/laid*100,failed.h=laid-hatched,failed.e=laid-emerged)

hatch14 <- hatch14 %>% dplyr::select(nest,lat,long,location, zone,year, laid, emerged, hatched,emerge.success,hatch.success,)
