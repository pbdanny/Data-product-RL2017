## -------------
## RL Analysis 
## ------------- 

## Load data & change column name to R compatible format ----
file <- file.choose()
rl2017 <- read.delim(file, header = T, na.strings = c("NA", ""), stringsAsFactors = F)
colnames(rl2017) <- make.names(colnames(rl2017))

## 1) Data sanitation check ----
# 1.1 check reasonable data type
# found Criteria_Code, Occupation_Code, Month need conversion to
# charactor type
rl2017$Criteria_Code <- as.character(rl2017$Criteria_Code)
rl2017$Occupation_Code <- as.character(rl2017$Occupation_Code)
rl2017$Month <- as.character(rl2017$Month)

# 1.2 Check outlier
# Found extreme outliner Monthly_Salary = 18MMTHB/Mth
# Directly Remove the record, since others varible
# shown irreasonable,
rl2017 <- rl2017[rl2017$Monthly_Salary < 5000000,]

# Found AGE <- 0, use mice to imputate age
library(mice)
# MICE work for data = NA , then convert AGE<0 to NA
rl2017[rl2017$AGE < 0,]$AGE <- NA
# Create new Dataframe only varible use to imputate
df <- rl2017[c("ZipCode", "Monthly_Salary", "AGE", "Occupation_Code")]
# Create imputated object name 'imp'
imp <- mice(df)
# use 'complete' command to createcompleted imputated DataFrame
# from imputate object
rl2017$AGE <- complete(imp)$AGE

# 1.3 Clear unused dataframe
rm(list = c("df", "imp"))

# 1.4 Create varible used in modelbuilding
# channel {'Direct', 'Tele'}
rl2017$channel <- ifelse(rl2017$Source_Code %in% 
                           c('OBB','OBS','OBU','OCB','OCS','OES',
                             'OGS','OSI','OSS','OSU','AXA'),
                         'Direct', 'Tele')

# 1.5 Sove dataframe 
saveRDS(rl2017, file = "/Users/Danny/Share Win7/rl2017.RDA")

## 2) EDA check ----
# Load Data
rl2017 <- readRDS(file = "/Users/Danny/Share Win7/rl2017.RDA")

# 2.1 Univariate plot 
# Finalized by source code
# Create contingency table count of Source Code, sorted
tableSource <- sort(table(rl2017$Source_Code))
# Barplot , create matrix 'barCenterX' : center of each bar in coordinate-x
# Extend plot area with ylim + 20% of max y value
barCenterX <- barplot(tableSource, ylim = c(0, 1.2*max(tableSource)))
# Annotated plot wtih text at position x = barCenterX
# y position = count of Source Code, lable = count of Source Code
# pos = 3, for label above y position specified
text(x = barCenterX, y = tableSource, labels = tableSource, pos = 3)
title(main = "Count of finalized by Source Code", sub = "data Jan. - Sep. 2017")

# 2.2 Bivariate plot

# From initail plot show outliner 
# filter out Monthly_Salary > 10000
library(ggplot2)

ggplot(data = subset(rl2017, Monthly_Salary > 1000),
       aes(x = AGE, 
           y = log10(Monthly_Salary), 
                     color = as.factor(Result))) +
  geom_jitter(alpha = 0.5)

## 3) Visualization ----

# 3.1 Geographical plot ----
# 3.1.1 Package 'rgdal : R Geospatial Data Abstraction Library' ----
# View & manipulate geospatial data

library(rgdal)

# Define directory of GIS dataset (shape file)
# consist of many files with extension .prj, .dbf, .shx and .shp 
shapeDataDir <- "/Users/Danny/Documents/Shapefile_tha_adm1_gista_plyg_v5"

# Define layer name; group of same file name with different extenstion above
thaiAdm2Layer <- "THA_Adm1_GISTA_plyg_v5"

# Load shape file
thaiMaps <- readOGR(dsn = shapeDataDir, layer = thaiAdm2Layer)

# Show varible in shapefile
summary(thaiMaps)

# Example : filter information from shapefile
# the information stored in object @data, dataframe format 
# access in @object$colum name style
thaiMaps[thaiMaps@data$Adm1Name == "Chon Buri",]@data

# or direct access dataframe column name
thaiMaps[thaiMaps$Adm1Name == "Chon Buri",]@data

# plot map data by 'plot' command
plot(thaiMaps, col = "lightgray", lty = 3, lwd = 0.5)

detach(package:rgdal)

# 3.1.2 Package 'tmap : Thematic map' ----
# Beautiful plot maps data

library(tmap)

# Load shape file with 'rgdal' package
# use 'readOGR' since more parameter to manipulate 
# encoding than tmapstool::read_shape
# thaiMapsFull <- tmaptools::read_shape(file = "/Users/Danny/Documents/Shapefile_tha_adm1_gista_plyg_v5/THA_Adm1_GISTA_plyg_v5.shp")

thaiMapsFull <- rgdal::readOGR(dsn = "/Users/Danny/Documents/Shapefile_tha_adm1_gista_plyg_v5",
          layer = "THA_Adm1_GISTA_plyg_v5", stringsAsFactors = FALSE, 
          encoding = "UTF-8", use_iconv = TRUE)

# Speed-up polygon plotting with simplify shapefile
# from 'rgeos::gSimplify'; but this command will not copy @data to
# use 'sp::SpatialPolygonsDataFrame', create new shapefile from 
# combinning simplify version + original data in @data
simMaps <- rgeos::gSimplify(thaiMapsFull, tol = 0.1, topologyPreserve = TRUE)
thaiMaps <- sp::SpatialPolygonsDataFrame(simMaps, data = thaiMapsFull@data)
rm('simMaps')
# store 'thaiMaps' as R named object '.Rdata'
save(thaiMaps, file = "thai shapefile adm1 simplify.Rdata")
# load back 'thaiMaps'
load(file = "thai shapefile adm1 simplify.Rdata")

# Check if number of polygons same as original version
length(thaiMapsFull) == length(thaiMaps)

# Save back simplify shapefile data
# Create folder for store all shapefile data
# dir.create("thai shapefile adm 1 simplify")
# anyway store the @data could not do in "UTF-8"
# rgdal::writeOGR(obj = thaiMaps, dsn="thai shapefile adm 1 simplify", 
#                 layer="thai_adm1_simplify", driver="ESRI Shapefile",
#                 encoding = "UTF-8")
# test <- rgdal::readOGR(dsn="thai shapefile adm 1 simplify", 
#                        layer="thai_adm1_simplify",
#                        encoding = "UTF-8")


# Create aggregated finalized data by channel and zipcode
rl2017 <- readRDS(file = "/Users/Danny/Share Win7/rl2017.RDA")

# Count record (use fn 'length') by channel + ZipCode 
finalize_channel_zipcode <- aggregate(data = rl2017,
                                      Region ~ channel + ZipCode,
                                      FUN = length)
# Rename 'Region' to 'finl'
finalize_channel_zipcode$finl <- finalize_channel_zipcode$Region; finalize_channel_zipcode$Region <- NULL

# Mapping ZipCode to create province_eng name
zipProv <- readxl::read_excel("/Users/Danny/Documents/R Project/THA_adm/province.xlsx", sheet = 4)

# Find unmatched province
zipProv[!zipProv$Province_Eng %in% thaiMaps$Adm1Name,]

# Change province spelling to match with map data
zipProv[zipProv$Province_Eng == "Bangkok Metropolis", ]$Province_Eng <- 'Bangkok'

# create first 2 digit of zipcode for mapping 
finalize_channel_zipcode$zip2digits <- substr(finalize_channel_zipcode$ZipCode, 
                                              1, 2)

# create dataframe by left join finalized data with zipcode data 
# 'left join' by merge with parameter 'all.x' = TRUE
finl_zip_map <- merge(x = finalize_channel_zipcode, y = zipProv, 
                      by.x = "zip2digits", by.y = "Left2_Zipcode",
                      all.x = TRUE)
# filter out not match data
finl_zip_map <- subset(finl_zip_map, !is.na(Province_Eng))

# aggregrate data to province level to match with map layer
finl_prov <- aggregate(data = finl_zip_map,
                       finl ~ Province_Eng + Region_Eng, FUN = sum)

# Add 'Saraburi' in dataframe for map plotting\
finl_prov <- rbind(finl_prov, data.frame(Province_Eng = "Saraburi", 
                                         Region_Eng = "C",
                                         finl = 0))
# Create new shape file for ploting
# use package dplyr since the 'merge' shuffle plot order
library(dplyr)
thaiMaps_ktc <- thaiMaps
thaiMaps_ktc@data <- left_join(x = thaiMaps_ktc@data, y = finl_prov,
                               by = c("Adm1Name" = "Province_Eng"))
detach(package:dplyr)

# plot map with qtm (quick plot)
qtm(thaiMaps_ktc, fill = "finl", fill.palette = "div", 
    title = "Finalized by Province", 
    text = "Adm1Name", text.size = "finl", text.root = 2, fill.textNA = "NA")

# Plot and facet by region
tm_shape(thaiMaps_ktc) +
  tm_fill(col = 'finl', palette = 'Blues', title = 'Finalized App') + 
  tm_facets(by = 'Region_Eng')
# tm_text(text = 'finl', size = 'finl')
# tm_text error when faceting map

# Create interactive map
# by create map object
thaiMaps_ktc_i <- tm_shape(thaiMaps_ktc) +  # Define map object
  # Define attribute to be filled
  tm_fill(col = "finl", alpha = 1, palette = "Blues", title = "Finalized App") +
  tm_borders() + # Add border
  tm_text(text = "Adm1Name", size = "finl")  +
  tm_view(text.size.variable = TRUE) # set text size be varible 

# Change from plot mode to view (interactive)
tmap_mode("view")

# Start interactive mode
thaiMaps_ktc_i

# 3.1.3 ggplots ----

# Shape file : convert to dataframe before use
# use function 'broom::tidy' to convert spatialdataframe to dataframe
load(file = "thai shapefile adm1 simplify.Rdata")
thaiMaps_df <- broom::tidy(thaiMaps)

# 'broom' remove all attribute data, mapping back by use polygon id
# in spatialdataframe as key for mapping attribute data in to map dataframe
# use 'slot' function to extract 'ID' from object then 
# add to @data 
thaiMaps$polyID <- sapply(slot(thaiMaps, "polygons"), 
                          function(x) slot(x, "ID"))

# mapped @data back by id 
# thaiMaps_df1 <- merge(x = thaiMaps_df, y = thaiMaps, by.x = "id", by.y = "polyID")

# change 'merge' to dplyr::inner_join, param. change thaiMaps -> thaiMaps@data 
thaiMaps_df <- dplyr::left_join(thaiMaps_df, thaiMaps@data, 
                                by = c("id" = "polyID"))

# merge with ktc dataframe 
thaiMaps_df_ktc <- dplyr::left_join(x = thaiMaps_df, y = finl_prov,
                               by = c("Adm1Name" = "Province_Eng"))
# ggplot2
# aesthetic x = longitude : y = latitude & group = group
ggplot(data = thaiMaps_df_ktc) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = finl)) +
  coord_equal()

# 3.1.4 ggmaps ----
library(ggmap)

# Raster format : Download map from GoogleMaps.
# Raster format = bitmap file, could not scales or manipulated.
# Used as bass map for overlay with data plot
thaiMaps <- get_map(location = "Thailand", maptype = "roadmap")
ggmap(thaiMaps)

# Re-create summary data by Zipcode & Region
library(dplyr)
rl2017 <- readRDS(file = "/Users/Danny/Share Win7/rl2017.RDA")
finalize_zipcode <- aggregate(data = rl2017,
                              Source_Code ~ ZipCode + Region, FUN = length)
zip_lat_long <- readxl::read_excel("/Users/Danny/Share Win7/R Script/thaizip long lat.xlsx", sheet = 1)
zip_region <- readxl::read_excel("/Users/Danny/Documents/R Project/THA_adm/province.xlsx", sheet = 4)
zip_lat_long$ZipCode <- as.character(zip_lat_long$zip)
zip_lat_long$left_2digits_zip <- substr(zip_lat_long$zip, 1, 2)
zip_lat_long <- left_join(x = zip_lat_long, y = zip_region,
                 by = c("left_2digits_zip" = "Left2_Zipcode"))
lat_long_finalize <- left_join(x = zip_lat_long, y = finalize_zipcode)


# use 'qmplot' do overlay dotplot
qmplot(data = lat_long_finalize, x = lng, y = lat, maptype = "toner-lite",
       color = I("red"))

# use 'qmplot' do overlay density plot
qmplot(data = subset(lat_long_finalize, !is.na(Region)), 
       x = lng, y = lat, maptype = "toner-lite",
       color = I("red"), geom = "density2d") +
  facet_wrap( ~ Region_Eng)

# more decoration like ggplots
qmplot(data = lat_long_finalize, x = lng, y = lat, geom = "blank",
       maptype = "toner-background") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA)