library(tidyverse)

setwd("~/Desktop/Competition_DataSets/")

#..............................................................................
#create combined county data####
#..............................................................................
wbi_nccounty <- read_csv("~/Desktop/Competition_DataSets/Well-being_Data/WBI_NCCounty.csv")
names(wbi_nccounty) <- tolower(names(wbi_nccounty))

#change naming of counties
wbi_nccounty$county <- gsub(pattern = " County", "", x = wbi_nccounty$county)

#remove %s from values
rep_pct <- grep(pattern = "pct", x = names(wbi_nccounty), value = F)
test_rep <- test_wbi
test_rep[c(6)] <- gsub(pattern = "%", "", x = test_rep$pct_enjoyment)
test_rep$pct_enjoyment

#read in data file with county GEOIDs
county_summary <- read_csv("~/Desktop/EPID 701/county_birth_summary.csv")

county_summary <- county_summary %>% 
  select(county = county_name, GEOID = FIPS)
str(county_summary)

#join GEOIDs to wbi data
wbi_GEOID <- wbi_nccounty %>% 
  left_join(county_summary, by = "county")

#Read in SDOH data
SDOH_byCounty <- read_csv("~/Desktop/Competition_DataSets/SDOH_Data/SDOH_byCounty.csv")

head(SDOH_byCounty)
str(SDOH_byCounty)

nccounty_full <- wbi_GEOID %>% 
  left_join(SDOH_byCounty, by ="GEOID")
nccounty_full$bikewrkpct

write_csv(nccounty_full, path = "~/Desktop/NCcounty_full.csv")

class(wbi_GEOID$GEOID)
summary(wbi_GEOID$GEOID, na.rm=T)

summary(county_summary$GEOID)

summary(SDOH_byCounty$GEOID)

SDOH_byCounty$is_NC <- ifelse(test = SDOH_byCounty$GEOID == c(37001, 37199) , yes = 1, no = 0)

SDOH_byCounty[ ,SDOH_byCounty$is_NC == 1]

names(wbi_nccounty) %>% str_detect(string = "wellbeing")

county_summary$county <- as.factor(county_summary$county)

wbi_indices$county <- as.factor(wbi_indices$county)

str(wbi_indices)

wbi_indices <- wbi_nccounty %>% 
  dplyr::select(county,
                wellbeing_index,
                wellbeing_physical,
                wellbeing_community,
                wellbeing_financial,
                wellbeing_social,
                wellbeing_purpose)

#...................................................................................
#linear regressions####
#...................................................................................
plot(x = nccounty_full$bikewrkpct*100, y = nccounty_full$wellbeing_index)
lm(formula = wellbeing_index ~ bikewrkpct, data = nccounty_full)
cor(x = nccounty_full$bikewrkpct*100, y = nccounty_full$wellbeing_index)


#...................................................................................
#create maps of indices####
#...................................................................................
#make short data frame with only indices
wbi_indices <- wbi_GEOID %>% 
  dplyr::select(county,
                wellbeing_index,
                wellbeing_physical,
                wellbeing_community,
                wellbeing_financial,
                wellbeing_social,
                wellbeing_purpose,
                GEOID)
str(wbi_indices)

#read in shp file
library(sf)

nccounty_sf = st_read("~/Desktop/EPID 701/maps/NC-Counties-Simple/nc_counties_simplified.shp", stringsAsFactors = F)
nccounty_sf$GEOID <- as.numeric(nccounty_sf$GEOID)
str(nccounty_sf)

#join indices and shp
nccounty_indices_sf <- left_join(x = nccounty_sf,
                                 y = wbi_indices,
                                 by = "GEOID")

nccounty_sf %>% st_geometry() %>% plot()

nccounty_indices_sf = nccounty_indices_sf %>% 
  st_transform(3404)

nccounty_indices_sf %>% 
  filter(is.na(wellbeing_index))

#map of well being index
nccounty_indices_sf %>% 
  filter(!is.na(wellbeing_index)) %>% 
  ggplot() +
  geom_sf(aes(fill = wellbeing_index)) +
  theme_void() +
  scale_fill_gradient(low = "#fdc5c5", high = "#8d0303")




#...................................................................................
#STATE COMBINE
#...................................................................................