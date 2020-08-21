# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Author: Liujun Wang
#  University of Melbourne
#  SDI Major Assignment Group 8
#  Hospital Task
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
if(!require(rgdal)){install.packages("rgdal")}
if(!require(sf)){install.packages("sf")}
if(!require(sp)){install.packages("sp")}
if(!require(RPostgreSQL)){install.packages("RPostgreSQL")}
if(!require(raster)){install.packages("raster")}
if(!require(dodgr)){install.packages("dodgr")}
if(!require(osmdata)){install.packages("osmdata")}
if(!require(maptools)){install.packages("maptools")}
if(!require(Miniconda)){install.packages("Miniconda")}
if(!require(remotes)){install.packages("remotes")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(RQGIS3)){remotes::install_github("jannes-m/RQGIS3")}
if(!require(rgeos)){install.packages("rgeos")}
#END of library loading######################

########Preparing hospital data (from postgres)#################
setwd("D:/Unimelb/2020-s2/GEOM90015 - SDI/MA/R/test")#YOUR OUTPUT FOLDER
set_env("C:/Program Files/QGIS 3.10")#YOUR QGIS FOLDER
#create connection object to PostgreSQL
con <- dbConnect(drv =PostgreSQL(),
                 user="postgres",
                 password="8kd5x9v",
                 host="172.26.133.209",
                 port=5432,
                 dbname="sdi_ma_db")
dbListTables(con)#list all the tables
dsn = "PG:dbname='sdi_ma_db' host='172.26.133.209' 
      port='5432' user='postgres' password='8kd5x9v'"
#-----
#query the database and store hospital data in dataframe
hospital <- readOGR(dsn,layer = "hospital")
#-----

########Preparing road data (from OSM)#################
#get bounding box of melbourne Regions
melbNorth_bb <- getbb("Northern Metropolitan Region", #<-setting up region#
                      display_name_contains = "Metropolitan",
                      format_out = "matrix",
                      featuretype = "relation",
                      silent = TRUE)
melbWest_bb <- getbb("Western Metropolitan Region",
                     display_name_contains = "Metropolitan",
                     format_out = "matrix",
                     featuretype = "relation",
                     silent = TRUE)
melbSouth_bb <- getbb("Southern Metropolitan Region",
                      display_name_contains = "Metropolitan",
                      format_out = "matrix",
                      featuretype = "relation",
                      silent = TRUE)
melbEast_bb <- getbb("Eastern Metropolitan Region",
                     display_name_contains = "Metropolitan",
                     format_out = "matrix",
                     featuretype = "relation",
                     silent = TRUE)
melbSouthEast_bb <- getbb("South-Eastern Metropolitan Region",
                          display_name_contains = "Metropolitan",
                          format_out = "matrix",
                          featuretype = "relation",
                          silent = TRUE)
#-----
#get road network
memory.limit(size = 56000)#***increase memory size***
North_street <- dodgr_streetnet(melbNorth_bb,expand = 0.05)
West_street <- dodgr_streetnet(melbWest_bb,expand = 0.05)
South_street <- dodgr_streetnet(melbSouth_bb,expand = 0.05)
East_street <- dodgr_streetnet(melbEast_bb,expand = 0.05)
SouthEast_street <- dodgr_streetnet(melbSouthEast_bb,expand = 0.05)
#-----
#export road network -> EXPORT "melb_street.shp" (road network dataset)
North_street_sf <- North_street[c("osm_id","name","highway","foot","horse","wheelchair",
                                  "bicycle","motorcar","hgv","psv","geometry")]
West_street_sf <- West_street[c("osm_id","name","highway","foot","horse","wheelchair",
                                "bicycle","motorcar","hgv","psv","geometry")]
South_street_sf <- South_street[c("osm_id","name","highway","foot","horse","wheelchair",
                                  "bicycle","motorcar","hgv","psv","geometry")]
East_street_sf <- East_street[c("osm_id","name","highway","foot","horse","wheelchair",
                                "bicycle","motorcar","hgv","psv","geometry")]
SouthEast_street_sf <- SouthEast_street[c("osm_id","name","highway","foot","horse","wheelchair",
                                          "bicycle","motorcar","hgv","psv","geometry")]
North_street_sp <- as(North_street_sf, Class="Spatial")
West_street_sp <- as(West_street_sf, Class="Spatial")
South_street_sp <- as(South_street_sf, Class="Spatial")
East_street_sp <- as(East_street_sf, Class="Spatial")
SouthEast_street_sp <- as(SouthEast_street_sf, Class="Spatial")
st_write(North_street_sf, "north_street.gpkg")
st_write(West_street_sf, "west_street.gpkg")
st_write(South_street_sf, "south_street.gpkg")
st_write(East_street_sf, "east_street.gpkg")
st_write(SouthEast_street_sf, "southeast_street.gpkg")
#####
##read in regional street if already exists
# North_street_sf<-st_read("north_street.gpkg")
# West_street_sf<-st_read("west_street.gpkg")
# South_street_sf<-st_read("south_street.gpkg")
# East_street_sf<-st_read("east_street.gpkg")
# SouthEast_street_sf<-st_read("southeast_street.gpkg")
#####
melb_street <- do.call(rbind, list(North_street_sp,West_street_sp,South_street_sp,East_street_sp,SouthEast_street_sp))
melb_street_ds <- melb_street[which(!duplicated(melb_street$osm_id)), ]
writeOGR(melb_street_ds, dsn="melb_street.gpkg", layer="melb_street", driver="GPKG")
########END of data preparation#################

#-----
#Metropolitan Melbourne boundary
greatermelb_bb <- getbb("Greater Melbourne",
                        format_out = "sf_polygon",
                        featuretype = "relation",
                        silent = TRUE)
st_write(greatermelb_bb,"greatermelb_boundary.gpkg")
greatmelb_sp <- readOGR("greatermelb_boundary.gpkg")
#------
#get hospital inside of focus regions
melb_hospital <- intersect(hospital, greatmelb_sp)
melb_hospital_sf <- st_as_sf(melb_hospital, crs=4326)
#clip road to greater melbourne region
run_qgis(alg="saga:polygonclipping",
         CLIP="greatermelb_boundary.gpkg",
         S_INPUT="melb_street.gpkg",
         S_OUTPUT="melb_street_clip")
#-----
#build road network graph
melb_street_clip <- st_read("melb_street_clip.shp",geometry_column="geometry")
melb_street_ls <- st_cast(melb_street_clip,"LINESTRING")
melb_street_graph <- weight_streetnet(melb_street_ls)
#-----
#extract nodes from melb_st_graph as sf and sp
from_lonlat <- melb_street_graph[c("from_id","from_lon","from_lat")]
to_lonlat <- melb_street_graph[c("to_id","to_lon","to_lat")]
#clean from & to data
from_lonlat_cl <- from_lonlat[which(!duplicated(from_lonlat$from_id)), ]
to_lonlat_cl <- to_lonlat[which(!duplicated(to_lonlat$to_id)), ]
colnames(from_lonlat_cl) <- c("id","lon","lat")
colnames(to_lonlat_cl) <- c("id","lon","lat")
from_to_coords <- rbind(from_lonlat_cl,to_lonlat_cl)
from_to_coords_cl <- from_to_coords[which(!duplicated(from_to_coords$id)), ]
nodes <- st_as_sf(from_to_coords_cl, coords=c("lon","lat"), crs=4326)
st_write(nodes,"nodes.gpkg")
nodes_sp <- as(nodes, Class="Spatial")
#-----
#join nearest nodes
run_qgis(alg="qgis:distancetonearesthubpoints",
         INPUT=melb_hospital,
         HUBS=nodes_sp,
         FIELD="id",
         UNIT=0,
         OUTPUT="hubPoints")
hub_points <- readOGR("hubPoints.gpkg")
#-----
#**Be careful: following catchment calculation regularly takes four hours
#**to run, but can be done separately by changing values in for(...)
#input "hub_points"(hospital), transform "melb_street_ls"(road network)
crs(hub_points)<-CRS("+proj=longlat +datum=WGS84")
hub_point.sf<-st_as_sf(hub_points)
hub_point.sf$id<-1:nrow(hub_point.sf)
melb_street_ls.t<-st_transform(melb_street_ls,4978)
#loop... elements:(hub_point.sf,melb_street_ls.t)
for(i in hub_point.sf$id){
  print(paste0("Current working on hospital: ", i))
  dists_lim_3k=3000
  dists_lim_5k=5000
  dists_lim_10k=10000
  buff_lim=dists_lim_10k
  sample_hub<-hub_point.sf[i,]
  sample_hub.t<-st_transform(sample_hub,4978)
  sample_hub.buffer<-st_buffer(sample_hub.t,buff_lim)
  sample_hub.buffer.t<-st_transform(sample_hub.buffer,4978)
  sample_street<-st_intersection(melb_street_ls.t,sample_hub.buffer.t)
  sample_street.t<-st_transform(sample_street,4326)
  sample_street_ls <- st_cast(sample_street.t,"LINESTRING")
  try({
    ddgraph<-weight_streetnet(sample_street_ls)
    
    #create nodes
    from_lonlat <- ddgraph[c("from_id","from_lon","from_lat")]
    to_lonlat <- ddgraph[c("to_id","to_lon","to_lat")]
    #clean from & to data
    from_lonlat_cl <- from_lonlat[which(!duplicated(from_lonlat$from_id)), ]
    to_lonlat_cl <- to_lonlat[which(!duplicated(to_lonlat$to_id)), ]
    colnames(from_lonlat_cl) <- c("nid","lon","lat")
    colnames(to_lonlat_cl) <- c("nid","lon","lat")
    from_to_coords <- rbind(from_lonlat_cl,to_lonlat_cl)
    from_to_coords_cl <- from_to_coords[which(!duplicated(from_to_coords$nid)), ]
    nodes <- st_as_sf(from_to_coords_cl, coords=c("lon","lat"), crs=4326)
    nodes_sp <- as(nodes, Class="Spatial")
    #----[end]----> nodes,nodes_sp
    fid <- st_nearest_feature(sample_hub,nodes)
    nid <- nodes[fid,]$nid
    
    route_distances <- dodgr_dists(ddgraph, from = nid)
    
    rt_dists_t <- t(route_distances)
    rt_dists <- as.data.frame(rt_dists_t)
    dists_id <- tibble(nid=rownames(rt_dists), distance=rt_dists[,1])
    dists_dlow_3k <- filter(dists_id,distance<=dists_lim_3k)
    dists_dlow_5k <- filter(dists_id,distance<=dists_lim_5k)
    dists_dlow_10k <- filter(dists_id,distance<=dists_lim_10k)
    inbound_st_3k <- merge(nodes,dists_dlow_3k,by="nid")
    inbound_st_5k <- merge(nodes,dists_dlow_5k,by="nid")
    inbound_st_10k <- merge(nodes,dists_dlow_10k,by="nid")
    concave_3k <- concaveman(inbound_st_3k,concavity=3)
    concave_3k$id <- i
    concave_3k$name <- sample_hub$labelname
    concave_5k <- concaveman(inbound_st_5k,concavity=3)
    concave_5k$id <- i
    concave_5k$name <- sample_hub$labelname
    concave_10k <- concaveman(inbound_st_10k,concavity=3)
    concave_10k$id <- i
    concave_10k$name <- sample_hub$labelname
    if(i==1){
      cover_3k<-concave_3k
      cover_5k<-concave_5k
      cover_10k<-concave_10k
    }
    cover_3k <- rbind(cover_3k,concave_3k)
    cover_5k <- rbind(cover_5k,concave_5k)
    cover_10k <- rbind(cover_10k,concave_10k)
  }, silent=TRUE)
}
#-----
#**clear data
cover_3k_sp <- as(cover_3k,Class="Spatial")
cover_5k_sp <- as(cover_5k,Class="Spatial")
cover_10k_sp <- as(cover_10k,Class="Spatial")
cover_3k_c <- gBuffer(cover_3k_sp, byid=TRUE, width=0)
cover_3k_sf <- st_as_sf(cover_3k_c)
cover_5k_c <- gBuffer(cover_5k_sp, byid=TRUE, width=0)
cover_5k_sf <- st_as_sf(cover_5k_c)
cover_10k_c <- gBuffer(cover_10k_sp, byid=TRUE, width=0)
cover_10k_sf <- st_as_sf(cover_10k_c)
#save coverage data
st_write(cover_3k_sf,"cover_3k.shp")
st_write(cover_5k_sf,"cover_5k.shp")
st_write(cover_10k_sf,"cover_10k.shp")
cover_3k <- st_read("cover_3k.shp")
cover_5k <- st_read("cover_5k.shp")
cover_10k <- st_read("cover_10k.shp")
# #save road catchment data (may take long time)
# #3k
# street_3k <- st_intersection(melb_street_clip,cover_3k)
# street_3k <- street_3k[which(!duplicated(street_3k$osm_id)), ]
# street_3k$fid <- NA
# street_3k$fid<-1:nrow(street_3k)
# st_write(street_3k,"street3k.gpkg")
# #5k
# street_5k <- st_intersection(melb_street_clip,cover_5k)
# street_5k <- street_5k[which(!duplicated(street_5k$osm_id)), ]
# street_5k$fid <- NA
# street_5k$fid<-1:nrow(street_5k)
# st_write(street_5k,"street5k.gpkg")
# #10k
# street_10k <- st_intersection(melb_street_clip,cover_10k)
# street_10k <- street_10k[which(!duplicated(street_10k$osm_id)), ]
# street_10k$fid <- NA
# street_10k$fid<-1:nrow(street_10k)
# st_write(street_10k,"street10k.gpkg")
########END of hospital service catchment#################

#calculate population
  #data input
pop_vic <- st_read(dsn,layer = "vic_age_sa2")
greatmelb <- st_read("greatermelb_boundary.gpkg")
greatmelb.t<- st_transform(greatmelb,4326)
pop_vic.t <- st_transform(pop_vic,4326)
pop_melb <- st_intersection(pop_vic.t,greatmelb.t)
pop_melb.sp <- as(pop_melb,Class="Spatial")
pop_melb$area_sqm <- raster::area(pop_melb.sp)
cover_3k <- st_read("cover_3k.gpkg")
cover_5k <- st_read("cover_5k.gpkg")
cover_10k <- st_read("cover_10k.gpkg")
hospital <- st_read(dsn,layer = "hospital")
hospital_clear <- hospital[,c("fid","labelname","roadname","postcode")]
  #calculate
pop_3k <- st_intersection(pop_melb,cover_3k)
pop_3k.sp <- as(pop_3k,Class="Spatial")
pop_3k$area2_sqm <- raster::area(pop_3k.sp)
pop_3k$pop_in_3k <- as.integer(pop_3k$total*(pop_3k$area2_sqm/pop_3k$area_sqm))
pop_in_3k <- aggregate(pop_3k$pop_in_3k, by=list(labelname=pop_3k$name), FUN=sum)
pop_in_3k_c <- pop_in_3k[pop_in_3k$x != 0, ]
pop_5k <- st_intersection(pop_melb,cover_5k)
pop_5k.sp <- as(pop_5k,Class="Spatial")
pop_5k$area2_sqm <- raster::area(pop_5k.sp)
pop_5k$pop_in_5k <- as.integer(pop_5k$total*(pop_5k$area2_sqm/pop_5k$area_sqm))
pop_in_5k <- aggregate(pop_5k$pop_in_5k, by=list(labelname=pop_5k$name), FUN=sum)
pop_in_5k_c <- pop_in_5k[pop_in_5k$x != 0, ]
pop_10k <- st_intersection(pop_melb,cover_10k)
pop_10k.sp <- as(pop_10k,Class="Spatial")
pop_10k$area2_sqm <- raster::area(pop_10k.sp)
pop_10k$pop_in_10k <- as.integer(pop_10k$total*(pop_10k$area2_sqm/pop_10k$area_sqm))
pop_in_10k <- aggregate(pop_10k$pop_in_10k, by=list(labelname=pop_10k$name), FUN=sum)
pop_in_10k_c <- pop_in_10k[pop_in_10k$x != 0, ]

hospital_pop1 <- merge(x = hospital_clear, y = pop_in_3k_c, by = "labelname", all.y = TRUE)
hospital_pop1$fid <- NA
hospital_pop1$fid <- 1:nrow(hospital_pop1)
hospital_pop1$pop_in_3k <- hospital_pop1$x
hospital_pop1 <- hospital_pop1[-c(5)]
hospital_pop2 <- merge(x = hospital_pop1, y = pop_in_5k_c, by = "labelname", all.y = TRUE)
hospital_pop2$pop_in_5k <- hospital_pop2$x
hospital_pop2 <- hospital_pop2[-c(6)]
hospital_pop3 <- merge(x = hospital_pop2, y = pop_in_10k_c, by = "labelname", all.y = TRUE)
hospital_pop3$pop_in_10k <- hospital_pop3$x
hospital_pop3 <- hospital_pop3[-c(7)]
#save hospital data
st_write(hospital_pop3,"hospital_pop.shp")
dbDisconnect(con)
# rm(list = ls())
########END of hospital population#################

#---------------------------------------------------------
#upload to geoserver
#***PLEASE put "rgs_utils.R" in the same folder with your scrip***
#***and change default values (such as globalGSCredentials["gsRESTURL"]...)***

setwd("D:/Unimelb/2020-s2/GEOM90015 - SDI/MA/R")#R scrip FOLDER
source("rgs_utils.R")

setwd("D:/Unimelb/2020-s2/GEOM90015 - SDI/MA/R/test")#YOUR result FOLDER

#The shp file you want to upload (!!must be zipped!!)
filepath_0 <- "hospital_pop.zip"
filepath_1 <- "cover_3k.zip"
filepath_2 <- "cover_5k.zip"
filepath_3 <- "cover_10k.zip"

utils.addShp2DataStore(filepath_0)
utils.addShp2DataStore(filepath_1)
utils.addShp2DataStore(filepath_2)
utils.addShp2DataStore(filepath_3)
