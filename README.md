Classification of weekly cycling flow patterns


To effectively promote the cycling traffic, it is important to have knowledge about the usage pattern in different spatial areas. Therefore, in this study the data from the bicycle counting
network in Hessen which was built in 2022 and consists of 274 counting stations is used to create weekly traffic flow patterns of cyclists and examine the spatial distribution of trip
purposes based on the results. In the following the code to prepare, filter and analysis the code, as to test the valicity and stability of results and further analysis are described:


Folder MiD Datensauswertung

File 01: Analyzing the data of MiD (Mobilität in Deutschland). 
   - load selected columns of the data set "MiD"
   - remove data which is not needed in the analyze and rename columns and data
   - general settings, like setting the order of days and trip purposes and the color of the plots
   - performing different analysis and plot the results
      - weekly flow patterns ~ trip purposes (summed counts)
      - weekly flow patterns ~ trip purposes (relational counts)
      - weekly flow patterns ~ detailed leisure transport
      - daily weather ~ trip purposes
      - track length ~ trip purposes
      - track length of pupils ~ percentage of tracks


Folder Other figures:

File 01: Creating figures to explain basic concepts and methods of the analysis
   - Skewness of data sets
   - Dendrograms
   - Scree-plots
     
        
Folder Typisierung:

File 01: Install of needed packages for the cluster analysis

Files 02: Load and enter of data 

   02_01: Enter the dates of holidays and vacation days for the years 2022 and 2023in Hessen (Germany)
   
   02_02: Load the daily count data of the bicycle counting stations for the years 2022 and 2023
   
   02_03: Load the daily weather data at the bicycle counting stations for the years 2022 and 2023 
      02_03_01: Standardize the data structure of the excel files with the weather data before loading them in R
      02_03_02: Load the standardized excel files in R
      02_03_03: Analyze the influence of daily weather on the count data to determine filter values to clean the data set
Files 03: Cluster analysis and validation
      03_01: Method 1: Cluster analysis using set day values for each counting station:
         Loop through the count data and determine the combination of counting stations which have the same amount of daily count values with the highest value.
      03_02: Method 2: Cluster analysis using all day values for each counting station
         03_02_01: Filter the data and executing the cluster analysis 
            1. First filtering of the data:
               - remove NA-Values
               - remove days in holidays and vacation periods
               - remove days with average temperatues under 10 °C and with snowfall
               - remove days with unexpected high or low counts are removed by the adjusted boxplot method
               - remove counting station with a median of unter 10 over all days of the week or with a median of 0 on one day of the week
               - remove counting stations which doesnt have more than 8 count data on each day of the week
            2. Determine median weekly flow patterns for each counting station with the median of the dayly counts for each day of the week
            3. Analyze the weekly flow patterns for the counting stations with the single linkage cluster analysis. 
               Calculate euclidean distance and build groups of similar weekly flow patterns.
            4. Second filtering of the data: 
               - remove counting stations with an euclidean distance over 0.01 
               - remove counting stations with an euclidean distance of over 0.01 and unusueal weekly flow patterns
            5. Hierarchical cluster analysis:
               - Perform different types of hierarchical cluster analysis
               - plot the results of the cluster analysises in a dendrogram
               - determine the ideal number of cluster groups with the dendrogram
            6. Kmeans cluster analysis:
               - loop the kmeans cluster analysis with 100 iterations and save the result with the highest average silhouette width (parameter for the stability of the results)
               - save and plot the values of the weekly flow patterns for each cluster group
               - analyze weekly flow patterns without a segure distribution to the groups and decide to remove them from analysis or not              
         03_02_02: Validation of the results of the cluster analysis:
            - Calculate the Rand-index, silhouette width and statistical parameters of the typical weekly flow patterns
            - evaluate the quality of the cluster analysis and compare it at different kind of processes
         03_02_02: Test of the stability of the results of the cluster analysis
            - loop with 100 iteration which splits data set in half and performs the whole cluster analysis with filtering of the data and calculation of the typical weekly flow patterns
            - save all results in one table
            - plot all results and exermine the variance of the results to evaluete the stability and quality of the result of the cluster analysis      


Folder Spatial analysis

File 01: Spatial analysis of the distribution of the counting stations to the cluster groups with typical weekly flow pattern
   - load the data, remove counting stations which was not grouped and general settings like the plot colors
   - analyze the relation of the cluster grouos with different spatial parameters and plot the results:
      - cluster group ~ RegioStaRGem5
      - cluster group ~ distance to schools
      - cluster group ~ distance to schools in urban area
      - cluster group ~ distance to river
      - cluster group ~ population density in surrounding 1 km raster cell
      - cluster group ~ track type normal and in percentage
      - cluster group ~ distance urban area
      - cluster group ~ net category of bicycle track
     
