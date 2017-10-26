#An algorithm to compute areas of geomorphic features in an EEZ or ECOREGION area and in its intersecting Marine Protected Areas
#(MPAs). For more detail about algorithm and its input setting can be found here: https://goo.gl/TiDhjq

#Script Name: Mpa_Intersect_V3.
#Category: BLUEBRIDGE PROJECT VRE
#R-Version:R 3.3.0  
#
# Authors:
#   Emmanuel Blondel (FAO),
#   Levi Westerveld (UNEP GRID-ARENDAL)
#   Debhasish Bhakta (UNEP GRID-ARENDAL)


#=============================================================================
# PACKAGE DEPENDENCIES
#=============================================================================
cat("Starting...\n")
require(RFigisGeo)
require(parallel)
require(jsonlite)
require(httr)
require(RCurl)
require(XML)
require(stringr)
require(rgeos)
require(raster)

#=============================================================================
# INPUTS DEFINITION
#=============================================================================

#Upload a zipped MPA shapefile. Shapefile must contain “name” attribute field with no duplicate values
MPA_Shapefile_Url <- "https://absences.zip"

#Type of marine boundary e.g. EEZ or ECOREGION
Marine_Boundary ="EEZ"

#Provide a unique regional id related to EEZ or ECOREGION e.g 5677
Region_Id = "NA"

#Provide a array of features only participate in geoprocessing in  eg. reefs,shelf,slope (optional)
Selected_Data_Feature="NA"


#=============================================================================
# INTERNAL VARIABLES
#=============================================================================

#check sys locales
cat("Checking system locales \n")
sys <- Sys.getlocale()
cat(sprintf("Default System locale = '%s' \n", sys))
if(sys == "C"){
  syslocale <- "en_US.UTF-8"
  cat(sprintf("Setting 'LC_ALL' category to '%s' \n", syslocale))
  Sys.setlocale(category = "LC_ALL", locale = syslocale)
  sys <- Sys.getlocale()
  cat(sprintf("New System locale = '%s' \n", sys))
}

#options
options(stringsAsFactors = FALSE)

#debugging
debugMode <- TRUE

#to run in parallel
runParallel <- TRUE
#totalCores <- 8
totalCores = detectCores()
securityCoreNb <- 1
#totalCores <- detectCores()
#if(totalCores > 8) securityCoreNb <- 2
runCores <- totalCores - securityCoreNb
cat(sprintf("Running with %s cores on %s cores availables \n", runCores, totalCores))

#spatial data
#selected_area <- NULL
#selected_intersect <- NULL

#bbox used to fetch geomorphic features
target_bbox <- NULL

#geomorphic types
baselayers <- c("shelf", "slope", "abyss", "hadal")
featurelayers <- c("reefs","seamounts", "guyots", "canyons", "ridges",
                   "plateaus", "spreading_ridges", "rift_valleys",
                   "glacial_troughs", "shelf_valleys", "trenches",
                   "troughs", "terraces", "fans", "rises",
                   "bridges","escarpments")
gtypes <- c(baselayers, featurelayers)

#list of geomorphic types for which bbox should be ignored
gtypesIgnoringBbox <- c("abyss")

#use Eckert IV equal area projection for area calculation
areaCRS <- CRS("+proj=eck4 +lon_0=Central Meridian +x_0=False Easting +y_0=False Northing")


#=============================================================================
# BUSINESS FUNCTIONS
#=============================================================================

#method to fetch a set of geomorphic features
# @param baseUrl object of class "character" the OWS baseUrl
# @param typeName object of class "character" layer name
# @param bbox object of class "matrix" representing a spatial bbox
# @param filter object of class "character" giving a CQL filter string
# @param verbose object of class "logical" Default is TRUE to print logs
# @returns an object of class "SpatialPolygonsDataFrame"
fetchFeatures <- function(baseUrl, typeName, bbox = NULL, filter=NULL, verbose = TRUE){
  #base config
  config <- list(
    baseUrl = baseUrl,
    service = "WFS",
    version = "1.0.0",
    request = "GetFeature",
    typeName = typeName
  )
  
  #optional bbox parameter
  if(!missing(bbox) && !is.null(bbox)){
    config$bbox = paste(bbox, collapse=",")
  }
  
  
  #optional filter
  if(!missing(filter)){
    config$cql_filter = filter;
    
    
  }
  
  
  request <- paste0(config$baseUrl,paste(lapply(2:length(config), function(i){paste(names(config)[i],config[[i]],sep="=")}),collapse="&"))
  
  if(verbose) cat("Reading WFS ", request,"\n")
  return(readWFS(request, target.dir = getwd(), verbose = verbose))
  
}

#method to fetch a set of geomorphic features
# @param typeName the layer name
# @param bbox a spatial bbox
# @param verbose TRUE to print logs
# @returns an object of class "SpatialPolygonsDataFrame"
fetchGeomorphicFeatures <- function(typeName, bbox, verbose = TRUE){
  return(
    fetchFeatures(
      baseUrl = "http://paim.d4science.org/geoserver/ows?",
      typeName = typeName, bbox = bbox,
      verbose = verbose
    )
  )
}



#method to fetch a complete dataset of geomorphic features
# @param gtypes object of class "character" the list of geomorphic types to retrieves
# @param gtypesIgnoringBbox object of class "character" the list of geomorphic types
#        for which bbox should be ignored for data fetching
# @param bbox object of class "matrix" giving a spatial bbox
# @param runParallel object of class "logical", if code has to be parallelized. Default is FALSE
# @param runCores object of class "integer". Number of cores to use. Default is 16
# @param verbose object of class "logical". Default is TRUE to print logs
# @returns an object of class "SpatialPolygonsDataFrame"
fetchGeomorphicFeaturesAll <- function(gtypes, gtypesIgnoringBbox, bbox,
                                       runParallel = FALSE, runCores = NULL,
                                       verbose = TRUE){
  
  #doFetch
  doFetch <- function(chunk_of_gtypes, gtypesIgnoringBbox, bbox, verbose){
    cat(sprintf("Fetching data for chunk [%s] \n", paste(chunk_of_gtypes,collapse=',')))
    cat("----------------------------------------------------------------------------\n");
    out <- lapply(
      chunk_of_gtypes,
      function(gtype){
        cat(sprintf("Fetching data for geomorphic type %s \n",gtype))
        
        #typeName
        typeName <- paste0("W_mpa:geo_fea_", gtype)
        
        #bbox
        layerBbox <- bbox
        if(gtype %in% gtypesIgnoringBbox){
          cat(sprintf("Ignoring bbox parameter for %s \n",gtype))
          layerBbox <- NULL
        }
        
        #fetching
        sp <- fetchGeomorphicFeatures(typeName, bbox, verbose = TRUE)
        if(!is.null(sp)) sp@data$gtype <- gtype
        return(sp)
      }
    )
    out <- out[!sapply(out,is.null)]
    out <- do.call("rbind", out)
    return(out)
  }
  
  #chunks of gtypes
  chunks <- list(gtypes)
  if(runParallel) chunks <- suppressWarnings(split(gtypes, 1:runCores))
  
  #run fetching
  sp <- switch(as.character(runParallel),
               "FALSE" = lapply(chunks,
                                doFetch, gtypesIgnoringBbox, bbox, verbose),
               "TRUE"  = mclapply(chunks,
                                  doFetch, gtypesIgnoringBbox, bbox, verbose,
                                  mc.preschedule = TRUE, mc.cores = runCores)
  )
  sp <- sp[!sapply(sp,is.null)]
  sp <- do.call("rbind", sp)
  return(sp)
}

#method to intersect selected areas with geomorphic features
# @param areas1 object of class "SpatialPolygonsDataFrame"
# @param areas2 object of class "SpatialPolygonsDataFrame"
# @param crs object of class "CRS" giving the CRS to use fo surface area computation
# @param runParallel object of class "logical", if code has to be parallelized. Default is FALSE
# @param runCores object of class "integer". Number of cores to use. Default is 16
# @param verbose object of class "logical". Default is TRUE to print logs
# @returns an object of class "SpatialPolygonsDataFrame"
intersectAll <- function(areas1, areas2, crs,
                         runParallel = FALSE, runCores = NULL,
                         verbose = TRUE){
  
  #doIntersect
  doIntersect <- function(chunk_of_areas, areas1, areas2, crs){
    areas1_to_intersect = areas1[chunk_of_areas,]
    out <- intersection(areas1_to_intersect, areas2, areaCRS = crs)
    #out<- rgeos::intersect(areas1_to_intersect, areas2)
    return(out)
  }
  
  #chunks of areas
  if(runParallel){
    chunks <- suppressWarnings(split(row.names(areas1), 1:runCores))
    chunks <- chunks[sapply(chunks, function(x){length(x)>0})]
    nbChunks <- length(chunks)
    if(nbChunks < runCores){
      runCores <- nbChunks
    }
  }
  
  #run intersect
  sp <- switch(as.character(runParallel),
               "FALSE" = intersection(areas1, areas2, areaCRS = crs),
               "TRUE"  = mclapply(chunks,
                                  doIntersect, areas1, areas2, crs,
                                  mc.preschedule = TRUE, mc.cores = runCores)
  )
  if(class(sp) == "list"){
    sp <- sp[!sapply(sp,is.null)]
    sp <- do.call("rbind", sp)
  }
  
  return(sp)                  
}

#Function to read shapefile from the zip folder and return spatial spatialpolygonsdataframe
# @param srcFile is zip folder

readingShapeFile =  function(srcFile){
  
  
  #downloding zip file and rename to original name
  fil = GET(srcFile,write_disk("tmp.zip",overwrite = TRUE))
  
  srcFile = str_match(headers(fil)$'content-disposition', "\"(.*)\"")[2]
  srcFile = tools::file_path_sans_ext(srcFile)
  srcFile = paste0(srcFile,"_",format(Sys.time(),"%Y%m%d%H%M%S"),".zip")
  file.rename("tmp.zip", srcFile)
  
  files <- unzip(srcFile, list = TRUE)$Name #list files only
  unzip(srcFile, exdir = getwd()) #proceed with unzip
  cat(sprintf("Unzipped files: [%s] \n", paste(files, collapse=",")))
  shp <- files[regexpr(".shp",files)>0][1]
  cat(sprintf("Main ESRI shapefile filename: '%s' \n", shp))
  filename <- unlist(strsplit(shp, ".shp"))[1]
  cat(sprintf("Filename: '%s' \n", filename))
  cat(sprintf("Check ESRI shapefiles in current working directory: [%s] \n", paste(list.files(pattern = filename), collapse=","))) 
  sp <- readOGR(dsn = shp, layer = filename)
  
  #Make all the coloum to lowecast
  colnames(sp@data) =(tolower(colnames(sp@data)))
  
  #create or update gml_id 
  sp@data["gml_id"] = rownames(sp@data)
  
  #create type
  #filePath@data["type"] = shapName
  
  #select the required coloum
  mapCol =c("gml_id","name")
  sp@data = sp@data[,mapCol]
  
  return(sp)
}


#function to add remaing row and col in dataframe
add_remaining_col = function(NameDF_row = NULL, NameDF_col=NULL, updateReport = NULL)
{
  
  if(!is.null(NameDF_row))
  {
    NameDF_row = unique(NameDF_row)
    temprow <- matrix(c(rep.int(NA,length(updateReport))),nrow=1,ncol=length(updateReport))
    
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(updateReport)
    
    
    for(rowVal in NameDF_row){
      
      if(!(rowVal %in% updateReport$name)){
        
        newrow[colnames(newrow)[1]] = rowVal
        newrow[colnames(newrow)[2]] = "MPA"
        
        updateReport <- rbind(updateReport ,newrow)
      }
    }
    
  }
  
  if(!is.null(NameDF_col))
  {
    NameDF_col = unique(NameDF_col)
    
    for(calVal in NameDF_col){
      if(!(calVal  %in% colnames(updateReport))){
        updateReport[calVal] <- 0
        
      }
    }
  }
  return(updateReport)
  
}



# method to convert extent to polygon as wfs insersect cql parameter
bbxPolygon = function(bbx){
  p1 =paste(bbx[1],bbx[4],sep=" ")
  p2 =paste(bbx[3],bbx[4],sep=" ")
  p3 =paste(bbx[3],bbx[2],sep=" ")
  p4 =paste(bbx[1],bbx[2],sep=" ")
  pAll = paste(p1,p2,p3,p4,p1,sep = ",")
  pPoly = paste("Intersects(geom,POLYGON((",pAll,")))")
}



#=============================================================================
# BUSINESS ALGORITHM
#=============================================================================

#check the array for Data Feature
if(Selected_Data_Feature != "NA" && all((Selected_Data_Feature = unlist(strsplit(Selected_Data_Feature,","))) %in% gtypes))
{
  
  gtypes = Selected_Data_Feature
  
}else
{
  cat("List contain invalid name")
}


## identify the combination switchNo for intersection####
#variable to store intersection pattern
if(MPA_Shapefile_Url == "https://absences.zip" && Marine_Boundary =="EEZ" && Region_Id != "NA")
{
  #intersection bwt EEZ  and predefine Geomorphicfeature
  switchNo ="1001"
  
}else if(MPA_Shapefile_Url == "https://absences.zip" && Marine_Boundary =="ECOREGION" && Region_Id != "NA"){
  
  #intersection bwt ECOREGION  and predefine Geomorphicfeature
  switchNo ="1002"
  
}else if(MPA_Shapefile_Url != "https://absences.zip" ){
  
  #intersection bwt  user define MPA  and predefine Geomorphic feature
  #regardless of Marine_Boundary and Region_Id
  
  switchNo ="1003"
  
}else{
  
  
  switchNo ="0000"
  
  
}


# Reading and fetching require files for intersection
switch(switchNo,
            
       "1001" = {
         
         #fetch the area of EEZ
         Region_Area = fetchFeatures(baseUrl = "http://paim.d4science.org/geoserver/W_mpa/wms?",
                                     typeName = "W_mpa:eez",filter = sprintf("mrgid_eez = '%s'", Region_Id, verbose = debugMode) )
         
         
         getCol =c('gml_id',"geoname","mrgid")
         Region_Area@data = Region_Area@data[,getCol]
         
         #defineType in Region_Area
         Region_Area@data$type = "EEZ"
         
         Region_Area@data["name"] = paste(Region_Area@data$mrgid,"-<-",Region_Area@data$geoname,sep = "")
         
         mapCol =c("gml_id","name","type")
         Region_Area@data = Region_Area@data[,mapCol]
         
         target_bbox <- slot(Region_Area, "bbox")
         
         #fetch the mpa based on EEZ id
         cat("Fetching MPA from intersect_mpa_eez layer\n")
         
         Marine_Boundary = fetchFeatures(
           baseUrl = "http://paim.d4science.org/geoserver/W_mpa/wms?",
           typeName = "W_mpa:intersect_mpa_eez_v1",
           filter = sprintf("mrgid_eez = '%s'", Region_Id, verbose = debugMode))
         
         
         
         if(length(Marine_Boundary) > 0 ){
         #compile name column to genrate unique
         Marine_Boundary@data["name"] = paste(Marine_Boundary@data$wdpaid,"-<-",Marine_Boundary@data$name,sep = "")
         
         
         #defineType in Marine_Boundary
         Marine_Boundary@data$type = "MPA"
         
         #select the required coloum
         mapCol =c("gml_id","name","type")
         Marine_Boundary@data = Marine_Boundary@data[,mapCol]
         
         
         #target bbox for the analysis
         #target_bbox <- slot(Marine_Boundary, "bbox")
         
         
         #load shapefiles of global scale geomorphic features to be intersected with EEZ and selected MPA
         cat("Fetching geomorphic features\n")
         system.time(
           geoFeatures <- fetchGeomorphicFeaturesAll(
             gtypes, gtypesIgnoringBbox, target_bbox,
             runParallel = runParallel, runCores = runCores,
             verbose = debugMode
           )
         )
         
         
         Marine_Boundary = rbind(Region_Area, Marine_Boundary)
         
         
         #storing   refrence datasets to add the remaining row and col in report
         userMPA_colName=Marine_Boundary@data$name
         geoFeatures_colName=gtypes
         
         #Store surface of each MPA
         Marine_Surface = Marine_Boundary
         Marine_Surface@data$surface = gArea(spTransform(Marine_Boundary, areaCRS), byid=TRUE)
         Marine_Surface = Marine_Surface@data[,c("name","surface")]
         
         }else{
           
           switchNo ="0000"
           
         }
         
       },
       
       "1002" = {
         
         #fetch the area of ecoregion
         Region_Area = fetchFeatures(baseUrl = "http://paim.d4science.org/geoserver/W_mpa/wms?",
                                     typeName = "W_mpa:marine_ecoregions",filter = sprintf("ecoid = '%s'", Region_Id, verbose = debugMode) )
         
         getCol =c('gml_id',"ecoregion","ecoid")
         Region_Area@data = Region_Area@data[,getCol]
         
         #defineType in Region_Area
         Region_Area@data$type = "ECOREGION"
         
         Region_Area@data["name"] = paste(Region_Area@data$ecoid,"-<-",Region_Area@data$ecoregion,sep = "")
         
         mapCol =c("gml_id","name","type")
         Region_Area@data = Region_Area@data[,mapCol]
         
         target_bbox <- slot(Region_Area, "bbox")
         
         #fetch the mpa based on ecoregions id
         cat("Fetching MPA from intersect_mpa_ecoregions layer\n")
         
         
         Marine_Boundary = fetchFeatures(
           baseUrl = "http://paim.d4science.org/geoserver/W_mpa/ows?",
           #typeName = "W_mpa:union_edit_dis&propertyName=the_geom,name,ecoid,wdpaid",
           typeName = "W_mpa:intersect_mpa_marine_ecoregions_v1",
           filter = sprintf("ecoid = '%s'", Region_Id, verbose = debugMode))
         
         
         if(length(Marine_Boundary) > 0 ){
         
         
         #compile name column to genrate unique
         Marine_Boundary@data["name"] = paste(Marine_Boundary@data$wdpaid,"-<-",Marine_Boundary@data$name,sep = "")
         
         
         
         #defineType in Marine_Boundary
         Marine_Boundary@data$type = "MPA"
         
         #select the required coloum
         mapCol =c("gml_id","name","type")
         Marine_Boundary@data = Marine_Boundary@data[,mapCol]
         
         #target bbox for the analysis
         #target_bbox <- slot(Marine_Boundary, "bbox")
         
         
         
         #load shapefiles of global scale geomorphic features to be intersected with ecoregion and selected MPA
         cat("Fetching geomorphic features\n")
         system.time(
           geoFeatures <- fetchGeomorphicFeaturesAll(
             gtypes, gtypesIgnoringBbox, target_bbox,
             runParallel = runParallel, runCores = runCores,
             verbose = debugMode
           )
         )
         
         Marine_Boundary = rbind(Region_Area, Marine_Boundary)
         
         
         
         
         #storing   refrence datasets to add the remaining row and col in report
         userMPA_colName=Marine_Boundary@data$name
         geoFeatures_colName=gtypes
         
         
         #Store surface of each MPA
         Marine_Surface = Marine_Boundary
         Marine_Surface@data$surface = gArea(spTransform(Marine_Boundary, areaCRS), byid=TRUE)
         Marine_Surface = Marine_Surface@data[,c("name","surface")]
         
         switchNo ="1001"
         }
         else{
           
           switchNo ="0000"
           
         }
         
         
       },
       
       "1003" ={
         
         
         #reading shapefile
         Marine_Boundary = readingShapeFile(MPA_Shapefile_Url)
         
         
         
         #target bbox for the analysis
         target_bbox <- slot(Marine_Boundary, "bbox")
         
         
         
         Marine_Boundary@data["name"] = paste(Marine_Boundary@data$gml_id,"-<-",Marine_Boundary@data$name,sep = "")
         
         
         #defineType in Marine_Boundary
         Marine_Boundary@data$type = "MPA"
         
         
         #select the required coloum
         mapCol =c("gml_id","name","type")
         Marine_Boundary@data = Marine_Boundary@data[,mapCol]
         
         
         
         
         #load shapefiles of global scale geomorphic features to be intersected with EEZ and selected MPA
         cat("Fetching geomorphic features\n")
         system.time(
           geoFeatures <- fetchGeomorphicFeaturesAll(
             gtypes, gtypesIgnoringBbox, target_bbox,
             runParallel = runParallel, runCores = runCores,
             verbose = debugMode
           )
         )
         cat("Geomorphic features fetched\n")
         
         
         
         #Store surface of each MPA
         Marine_Surface = Marine_Boundary
         Marine_Surface@data$surface = gArea(spTransform(Marine_Boundary, areaCRS), byid=TRUE)
         Marine_Surface = Marine_Surface@data[,c("name","surface")]
         
         
         
         #storing  copy of refrence datasets to update row and col in report
         userMPA_colName=Marine_Boundary@data$name
         geoFeatures_colName=gtypes
         
         
       },"0000" ={
         
         switchNo ="0000"
       }
)


#switchNo ="0000"
#intersect between MPA's and Geomorphic or user define feature , use areaCRS argument in RFigisGeo 
# tintersection to compute intersection area
if(length(Marine_Boundary) > 0 && switchNo != "0000"){
  cat("Intersecting areas\n")
  system.time(
    intersects <- intersectAll(Marine_Boundary,geoFeatures, areaCRS,
                               runParallel = runParallel, runCores = runCores,
                               verbose = debugMode)
    
  )
  
 cat("Intersection done\n")
  
}


#switchNo ="0000"

#Compiling report 
switch(switchNo,
       
       
       "1001" ={
         
         mapCol=c("name","gtype","type","geo_area")
         
         intersects@data = intersects@data[,mapCol]
         
         df = slot(intersects, "data")
         
         agg <- aggregate(  df$geo_area,  by=list(name=df$name, gtype=df$gtype,type=df$type),  FUN="sum")
         
         colnames(agg) <- c("name","gtype","type","area")
         
         agg = tidyr::spread(data = agg, key ="gtype" , value = "area")
         
        #merge the require surface
         agg = merge(agg,Marine_Surface,all=TRUE,by="name")
         
         #make type NA to MPA
         agg$type[is.na(agg$type)] <- "MPA"
         
         report = add_remaining_col(NameDF_row =userMPA_colName,NameDF_col = geoFeatures_colName, updateReport =agg  )
         
         #split the coloum in the layer
         report = tidyr::separate(report, "name", c("id","name"), sep = "-<-")
         
         
         #make all NA to 0
         invisible(sapply(colnames(report), function(x){if(class(report[,x])=="numeric"){ report[is.na(report[,x]),x]<<- 0 }}))
         
         #rearrange the col
         report = report %>% dplyr::select(1:3,surface, dplyr::everything())
         
         
         #adding all MPA
         system.time(
         allMPAs <- data.frame(
           id = 0,
           name = "All MPAs",
           type = "MPA",
           surface = gArea(spTransform(gUnaryUnion(Marine_Boundary[Marine_Boundary@data$type == "MPA",]), areaCRS)),
           do.call("cbind", lapply(colnames(report)[5:length(colnames(report))], function(x){
             sp <- intersects[intersects@data$gtype == x & intersects@data$type == "MPA",]
             out <- data.frame(var = 0)
             colnames(out) <- x
             if(length(sp)>0){
               sp <- gUnaryUnion(sp)
               out[1L,] <- raster::area(spTransform(sp, areaCRS))
             }
             return(out)
           }))
         )
         )
         #merge to report
         report <- rbind(report, allMPAs)
         
       },
       
       "1003" ={
         mapCol=c("name","gtype","type","geo_area")
         
         intersects@data = intersects@data[,mapCol]
         
         df = slot(intersects, "data")
         
         agg <- aggregate(  df$geo_area,  by=list(name=df$name, gtype=df$gtype,type=df$type),  FUN="sum")
         
         colnames(agg) <- c("name","gtype","type","area")
         
         agg = tidyr::spread(data = agg, key ="gtype" , value = "area")
         
         #merge the require surface
         agg = merge(agg,Marine_Surface,by="name")
         
         report = add_remaining_col(NameDF_row =userMPA_colName,NameDF_col = geoFeatures_colName, updateReport =agg  )
         
         #split the coloum in the layer
         report = tidyr::separate(report, "name", c("id","name"), sep = "-<-")
         
         
         #make all NA to 0
         invisible(sapply(colnames(report), function(x){if(class(report[,x])=="numeric"){ report[is.na(report[,x]),x]<<- 0 }}))
         
         #rearrange the col
         report = report %>% dplyr::select(1:3,surface, dplyr::everything())
         
         
       },
       "0000"={
         
         report = data.frame(c("Computation not possible with current selection or inputs"))
         colnames(report)=c("info")
       }
       
)


#=============================================================================
# OUTPUT DEFINITION
#=============================================================================

  cat("Producing output\n")
  #output
  #------
  #output file containing intersection result
  Report_Format = "json"
  #Plz uncomment in SAI
  outputFile<- paste0("output_report_", format(Sys.time(),"%Y%m%d%H%M%S"),".", Report_Format)
  switch(Report_Format,
         "csv"   = {
           fileCon =  write.table(report, outputFile, row.names = FALSE, sep=";")
         },
         "json"  = {
           file.create(outputFile)       
           conn <- file(outputFile)
           json <- toJSON(report, pretty = TRUE)
           fileCon =    writeLines(as.character(json), con = conn, sep = "\n", useBytes = FALSE)
           #close the connection
           close(conn)
         }
  )
