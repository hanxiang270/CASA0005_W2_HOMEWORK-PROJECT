#TASK:
#1. Merge the non-spatial data to spatial data
#2. Calculate the average percent of science students grades per county. meeting the required standards
#3. Produce a map to show where the Country Averages are above or below the State of Washington average

#Step 1. Data pre-processing
OG_NONSPATIAL<- read.csv("/Users/mac/Desktop/MSc Urban Spatial Science/CASA0005/W2/CASA0005_W2_HOMEWORK PROJECT/Report_Card_Assessment_Data_2018-19_School_Year_20241011.csv",header = TRUE, sep = ",", encoding = "latin1")
library(dplyr) #To activate '%>%' function
library(stringr) #To activate 'str_detect' function
OG_NONSPATIAL_SCIENCE<- OG_NONSPATIAL %>% 
  filter(str_detect(`TestSubject`, "Science")) #To filter rows that are only relevant to Science
OG_NONSPATIAL_SCIENCE <- OG_NONSPATIAL_SCIENCE %>%
  filter(str_detect(`StudentGroup`, "All Students")) #To filter rows that only represent all students
OG_NONSPATIAL_SCIENCE <- OG_NONSPATIAL_SCIENCE %>%
  filter(str_detect(`GradeLevel`, "All Grades")) #To filter rows that only represent all grades
OG_NONSPATIAL_SCIENCE <- OG_NONSPATIAL_SCIENCE %>%
  dplyr::select(contains("County"), contains("DistrictCode"), contains("DistrictOrganizationId"), contains("PercentMetStandard"), contains("TestAdministration")) #To filter out relevant columns
library(tidyr)
class(OG_NONSPATIAL_SCIENCE)
NONSPATIAL_DATATYPE <- OG_NONSPATIAL_SCIENCE %>% #To check the data type for each column
  + summarise_all(class)%>%
  + pivot_longer(everything(), names_to="All_variables", values_to="Variable_class")
OG_NONSPATIAL_SCIENCE <- OG_NONSPATIAL_SCIENCE %>%
  + distinct()
NONSPATIAL_WIDE <- OG_NONSPATIAL_SCIENCE %>%
  + pivot_wider(
    +id_cols = 1:3, 
    +names_from = TestAdministration,
    +names_sep = "_", 
    +values_from = PercentMetStandard,
    +values_fn = list(PercentMetStandard = dplyr::first) # This selects the first value when duplicates exist
  ) #Pivot table to put grades from different test administrations at the same row for each county
str(NONSPATIAL_WIDE) #To see the data type for each column
NONSPATIAL_WIDE <- NONSPATIAL_WIDE %>%
  +mutate(across(where(is.character), ~ gsub("<", "", .))) #To remove "<" in the character values
NONSPATIAL_WIDE <- NONSPATIAL_WIDE %>%
  +mutate(across(where(is.character), ~ gsub("<", "", .))) #To remove ">" in the character values
OG_NONSPATIAL_SCIENCE2 <- NONSPATIAL_WIDE %>%
  mutate(
    +WCAS_numeric = as.numeric(gsub("%", "", WCAS)),  # Remove '%' and convert to numeric
    +AIM_numeric = as.numeric(gsub("%", "", AIM))      # Remove '%' and convert to numeric
    + )
OG_NONSPATIAL_SCIENCE2 <- OG_NONSPATIAL_SCIENCE2 %>%
  + mutate(average_score = rowMeans(select(., WCAS_numeric, AIM_numeric), na.rm = TRUE))  # Average of WCAS and AIM for each row
FINAL_NONSPATIAL <- OG_NONSPATIAL_SCIENCE2 %>%
  +group_by(County) %>%
  +summarise(mean_average_score = mean(average_score, na.rm = TRUE))  # Calculate mean, ignoring NA values
average_score <- mean(FINAL_NONSPATIAL$mean_average_score)
FINAL_NONSPATIAL2 <- FINAL_NONSPATIAL %>%
  +mutate(county_difference = mean_average_score-44.068683845)%>%
  +mutate(across(county_difference, round,3))%>%
  +mutate(county_compare = case_when(mean_average_score >= 44.068683845 ~ str_c("equal or above Washington average by",county_difference,sep=""),TRUE ~ str_c("below Washington average by",county_difference,sep="")))

#Step 2. Merge Data
EW <- st_read("/Users/mac/Desktop/MSc Urban Spatial Science/CASA0005/W2/CASA0005_W2_HOMEWORK PROJECT/Washington_Counties_with_Natural_Shoreline___washsh_area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
install.packages("janitor")
library("janitor")
FINAL_MERGED <- EW %>%
  +left_join(FINAL_NONSPATIAL2, by = c("COUNTYLABE" = "County")) %>%
  +distinct(COUNTYLABE, .keep_all = TRUE)

#Step 3. Choropleth Map
library(tmap)
library(tmaptools)
library(OpenStreetMap)
OSM_WASHINGTON <- EW %>%
  +st_bbox(.) %>%  
  +tmaptools::read_osm(., type = "osm", zoom = NULL)
library(tmap)       # Ensure tmap is loaded
library(sf)         # For spatial data handling

# Assuming you have already created OSM_WASHINGTON and FINAL_MERGED correctly
tmap_mode("plot") # Set tmap to plot mode
map <- tm_shape(OSM_WASHINGTON) + # Base OSM
  tm_rgb() + # RGB layer for OSM
  tm_shape(FINAL_MERGED) + # Shape for the choropleth map
  tm_polygons("county_difference", # Fill based on county_difference
              style = "jenks", 
              palette = "YlOrBr", 
              midpoint = NA,
              title = "% Difference compared to state average", 
              alpha = 0.5) +
  tm_compass(position = c("left", "bottom"), type = "arrow") + # Compass
  tm_scale_bar(position = c("left", "bottom")) + # Scale bar
  tm_layout(title = "Washington State County-level Student Science Grade Comparison", 
            legend.position = c("right", "bottom")) # Layout

# Print the map
print(map)
###
