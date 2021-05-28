library("sf")
library("cartogram")
library("tidyverse")
library("lubridate")
library("tmap")
library("ggpubr")
library("glue")
library("gifski")


#shape files

download.file("https://maps.gov.scot/ATOM/shapefiles/SG_IntermediateZoneBdry_2011.zip", "int.zip")
unzip("int.zip")
scotlandintmap <- st_read("SG_IntermediateZone_Bdry_2011.shp") %>%
  select(InterZone, TotPop2011, geometry)





#covid data

download.file("https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/8906de12-f413-4b3f-95a0-11ed15e61773/download/trend_iz_20210523.csv", "covid.csv")

dfintchart <- read_csv("covid.csv", col_types=cols(
  Date = col_date(format="%Y%m%d"),
  IntZone = col_character(),
  IntZoneName = col_character(),
  CA = col_character(),
  CAName = col_character(),
  Positive7Day = col_character(),
  Positive7DayQF = col_character(),
  Population = col_double(),
  CrudeRate7DayPositive = col_character(),
  CrudeRate7DayPositiveQF = col_character()
))


#add council names

inttolaname <- dfintchart %>%
  group_by(IntZone) %>%
  select(CAName) %>%
  slice_head() %>%
  ungroup()

scotlandintmap <- scotlandintmap %>%
  left_join(inttolaname, by = c("InterZone" = "IntZone"))

# sort out rate variable

dfintchart <- dfintchart %>%
  mutate(CrudeRate7DayPositive=ifelse(is.na(CrudeRate7DayPositive), 
                                      "No rate - < 3 cases", CrudeRate7DayPositive)) %>%
  mutate(CrudeRate7DayPositive = ordered(CrudeRate7DayPositive,
                                         levels=c("No rate - < 3 cases", "1 to 49", "50 to 99", "100 to 199", "200 to 399",
                                                  "400+"))) 
  

#function to draw mps


carto <- function(df=scotlandintmap, place, size="TotPop2011") {
tomo <-  df %>%
  filter(CAName==place) %>%
  select(-CAName) %>%  
  cartogram_ncont(., size) %>% 
  right_join(select(dfintchart, Date, CrudeRate7DayPositive, IntZone)
            , by = c("InterZone" = "IntZone"))
topo<-tm_shape(tomo) +
    tm_polygons("CrudeRate7DayPositive", title="7 day covid rate",
                palette="BuPu") +
    tm_layout(title=glue("{place} neighbourhoods 
                         (scaled to population size)")) +
    tm_facets(along="Date") +
    tm_credits("Source: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/8906de12-f413-4b3f-95a0-11ed15e61773")
tmap_animation(topo, filename=glue("{place}.mp4"),
                 width = 1200, height = 600, loop=FALSE)
}

#draw maps

councils <- inttolaname %>%
  group_by(CAName) %>%
  slice_head() %>%
  ungroup() %>%
  select(CAName)


##this bit takes some time so don't run unless you have it and are prepared for crashes.
#Saving items as they are large so easier to 
##write and read back in when looping 
 
walk(councils$CAName, ~carto(place=.x))



