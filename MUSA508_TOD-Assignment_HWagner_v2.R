#---- Set Up ----

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions, styling for quantile breaks in the plots

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Load census API key

census_api_key("f65a8827c48f27546af2e646a3b4850e99cdd5c1", overwrite = TRUE)

# 2009 Census Data
tracts09_chi <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2009, state=17, county=031, geometry=T, output="wide") %>%
  st_transform(crs='ESRI:102671') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty)

# 017 Census Data -----

tracts17_chi <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state=17, county=031, geometry=T, output="wide") %>%
  st_transform(crs='ESRI:102671') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

# --- Combining 09 and 17 data ----

allTracts <- rbind(tracts09_chi,tracts17_chi)


# ---- Wrangling Transit Open Data -----

ctaStops <- 
  st_read("C:/Users/wagne/Documents/GitHub/Assignment1_ChicagoCTA/CTA_RailStations/CTA_RailStations.shp")

#Add a new column to ctaStops to aggregate line names

changelevels <- function(f, ...) {
  f <- as.factor(f)
  levels(f) <- list(...)
  f
}

ctaStops$Line <- 
  changelevels(ctaStops$LINES, 
               Blue=c("Blue Line", "Blue Line (Congress)", "Blue Line (O'Hare)", "Brown, Orange, Pink, Purple (Express), Green, Blue"), 
               Yellow=c("Yellow Line"),
               Red=c("Red Line", "Red, Brown, Purple (Express)", "Red, Yellow, Purple, Evanston Express"),
               Purple=c("Purple Line", "Purple Line, Evanston Express"),
               Pink=c("Pink"),
               Orange=c("Orange Line", "Orange & Green Lines"),
               Green=c("Green Line (Lake)", "Green Line (Englewood)", "Green Line", "Green (Lake), Pink"),
               Brown=c("Brown, Purple (Express)", "Brown, Orange, Pink, Purple (Express), Green, Blue", "Brown, Orange, Pink, Purple (Express)", "Brown, Orange, Pink,  Purple (Express), Green", "Brown Line"))

View(ctaStops)

ggplot() + 
  geom_sf(data=st_union(tracts09_chi)) +
  geom_sf(data=ctaStops, 
          aes(colour = Line), 
          show.legend = "point", size= 1) +
  scale_colour_manual(values = c("blue","yellow","red", "purple", "pink","orange","green","brown")) +
  labs(title="CTA Stops", 
       subtitle="Chicago, IL", 
       caption="Figure 2.5") +
  mapTheme()

# --- Relating CTA Stops and Tracts ----

ctaBuffers <- 
  rbind(
    st_buffer(ctaStops, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(ctaStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=ctaBuffers) + 
  geom_sf(data=ctaStops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

# ---- Spatial operations ----

# Consult the text to understand the difference between these three types of joins
# and discuss which is likely appropriate for this analysis

# Create an sf object with ONLY the unioned buffer
buffer <- filter(ctaBuffers, Legend=="Unioned Buffer")

ggplot() +
  geom_sf(data=buffer)

# Clip the 2009 tracts ... by seeing which tracts intersect (st_intersection)
# with the buffer and clipping out only those areas
View(clip)

ggplot() +
  geom_sf(data=clip)

clip <- 
  st_intersection(buffer, tracts09_chi) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Do a spatial selection to see which tracts touch the buffer
selection <- 
  tracts09_chi[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature
selectCentroids <-
  st_centroid(tracts09_chi)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09_chi, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# ---- Indicator Maps ----

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent)) %>%
  mutate(pctBach.scaled = pctBachelors * 100)

# Small Multiple Plot: TOD/Non-TOD Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09_chi))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#Small Multiple Plot: TOD/Non-TOD Rent Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09_chi))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#Small Multiple Plot: TOD/Non-TOD Population Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09_chi))+
  geom_sf(aes(fill = q5(TotalPop))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "TotalPop"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Total Population 2009-2017") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#Small Multiple Plot: TOD/Non-TOD Household Income Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09_chi))+
  geom_sf(aes(fill = q5(MedHHInc))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedHHInc"),
                    name = "Median Household Income\n(Quintile Breaks)") +
  labs(title = "Median Household Income 2009-2017") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#Small Multiple Plot: TOD/Non-TOD Education Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09_chi))+
  geom_sf(aes(fill = q5(pctBach.scaled))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBach.scaled"),
                    name = "Percent with Bachelors Degree\n(Quintile Breaks)") +
  labs(title = "Percent of Population with a Bachelors Degree 2009-2017") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# --- TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# Let's make some comparisons and speculate about the willingness to pay
# and demographics in these areas 2009-2017 (see the 2000 data in the text too)

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

# --- TOD Indicator Plots ------

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

#########################################
#Graduated Symbol Map for Rent and Population within 0.5 mile of CTA Stations

ctaBuffers <- 
    st_buffer(ctaStops, 2640) %>%
      mutate(Legend = "Buffer")

station_tracts <- st_intersection(allTracts, ctaBuffers)

Station_summary <-
  station_tracts %>%
  st_drop_geometry() %>%
  group_by(year, STATION_ID) %>%
  summarize(Population = sum(TotalPop),
            Rent = mean(MedRent), na.rm = T)

CTAStops_data <-
  left_join(ctaStops,Station_summary)
  
ggplot(CTAStops_data)+
  geom_sf(data =st_union(tracts09_chi))+
  geom_sf(aes(size=Population), color="blue")+
  scale_size_continuous(range=c(0,8))+
  labs(title = "Total Population within 0.5 miles of each CTA Station")+
  facet_wrap(~year)+
  mapTheme()

ggplot(CTAStops_data)+
  geom_sf(data =st_union(tracts09_chi))+
  geom_sf(aes(size=Rent), color="blue")+
  scale_size_continuous(range=c(0,8))+
  labs(title = "Median Rent within 0.5 miles of each CTA Station")+
  facet_wrap(~year)+
  mapTheme()


##########################################
# Crime Data Analysis

#Crime data 2009
#Source: https://data.cityofchicago.org/Public-Safety/Crimes-2009/qqw2-hwkh
#Filter for Thefts only - can pick a different type if interested!
Crime09 <- 
  st_read("https://data.cityofchicago.org/resource/qqw2-hwkh.geojson") %>%
  st_transform(crs='ESRI:102671') %>%
  mutate(crime_year = "2009") %>%
  filter(primary_type == "THEFT") %>%
  dplyr::select(primary_type, crime_year) %>%
  dplyr::select(-geometry)

ggplot() + 
  geom_sf(data=Crime09)

#Crime data 2017
#Scource: https://data.cityofchicago.org/Public-Safety/Crimes-2017/d62x-nvdr
#Filter for Thefts only - can pick a different type if interested!
Crime17 <- 
  st_read("https://data.cityofchicago.org/resource/d62x-nvdr.geojson")%>%
  st_transform(crs='ESRI:102671') %>%
  mutate(crime_year = "2017") %>%
  filter(primary_type == "THEFT") %>%
  dplyr::select(primary_type, crime_year) %>%
  dplyr::select(-geometry)

ggplot() + 
  geom_sf(data=Crime17)

#Combine 2009 and 2017 Crime Data
AllCrime <- rbind(Crime09, Crime17)

ggplot() + 
  geom_sf(data=AllCrime)

#Join Crime with Census Tract
AllCrime_join <-
  st_join(AllCrime,allTracts.group) %>%
  filter(crime_year == year)%>%
  dplyr::select(primary_type, crime_year, year, GEOID, TOD)

#Count crimes in each census tract
Crime_tract <-
  st_drop_geometry(AllCrime_join) %>%
  group_by(GEOID,year) %>%
  summarize(Thefts=n())

#Count crimes by year and TOD
Crime_year_TOD<-
  st_drop_geometry(AllCrime_join) %>%
  group_by(TOD, year) %>%
  summarize(Thefts=n())
  
#Join Crime Counts with AllTracts.group
AllTracts.group.crime<-
  left_join(allTracts.group,Crime_tract,)

#Summary tables for crime
allTracts.Crime.Summary <- 
  AllTracts.group.crime %>%
  st_drop_geometry() %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Thefts = sum(Thefts, na.rm = T))

kable(allTracts.Crime.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.4")

allTracts.Crime.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.5")

#Small Multiple Plots
allTracts.Crime.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")
