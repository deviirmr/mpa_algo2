# MPA Algorithm Description


The algorithm will perform an intersection operation and compute the intersected area for each feature based on user inputs. 
The algorithm performs an intersection operation and computes the intersected area for each feature based on the user input data. Users can execute the algorithm with two possible combinations. (1) A user gives an input for ‘MPA_Shapefile_Url’ and ‘Selected_Data_Feature’ and the other variables are left empty to their default values. (2) A user gives an input for ‘Marine Boundary’, ‘Region_Id’ and ‘Selected_Data_Feature’ and the other variable is left to its default value.



The ‘Mpa_Shapefile_Url’ entry accepts a valid web link to a zip folder containing a shapefile in a GCS coordinate system. To create a web link, users can upload a zip folder with the data to the workspace in the VRE. The shapefile must have a text type field called ‘name’ containing no duplicate attributes. The .zip folder must contain the standard .shp, .shx, .prj and .dbf files. 
The ‘Selected_Data_Feature’ entry accepts a comma separated list of features to be included in the analysis. These can be: 
shelf,slope,abyss,hadal,reefs,seamounts,guyots,canyons,ridges,plateaus,spreading_ridges,rift_valleys,glacial_troughs,shelf_valleys,trenches,troughs,terraces,fans,rises,bridges,escarpments,reefs,seagrass,mangroves


For (2), a user should select a ‘Marine_Boundary’ option from the dropdown menu. This can either be an Exclusive Economic Zone (EEZ) or a Marine Ecoregion. The user must then select an appropriate ‘Regional_ID’ determining which EEZ or Ecoregion to select for the analysis. A list of possible entries can be found in the [Table-I&II](https://goo.gl/TiDhjq)

###### -**Please note that:**

###### **-A.)** If a user changes the default variable ‘MPA_Shapefile_Url’ with a valid url, then the algorithm will always report on the input shapefile regardless of whether a user has also defined a ‘Marine_Boundary’ and ‘Region_ID’. 

###### **-B.)** Based on the inputs by the user the algorithm can take a significant amount of time to complete. This holds especially true when analyzing large areas such as the Norwegian EEZ for example.

