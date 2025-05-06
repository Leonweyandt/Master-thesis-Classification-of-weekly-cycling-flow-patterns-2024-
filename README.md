Classification of weekly cycling flow patterns<br/><br/>


To effectively promote the cycling traffic, it is important to have knowledge about the usage pattern in different spatial areas. Therefore, in this study the data from the bicycle counting
network in Hessen which was built in 2022 and consists of 274 counting stations is used to create weekly traffic flow patterns of cyclists and examine the spatial distribution of trip
purposes based on the results. It follows a brieve discription of the R-code used to prepare, filter and analyze the data:<br/><br/>


Folder "Other figures":

File 01: Creating figures to explain basic concepts and methods of the analysis
   - skewness of data sets
   - dendrograms
   - scree plots<br/><br/>

     
Folder "Clustering":

File 01: Install packages for the cluster analysis

Files 02: Load and prepare data 

   - 02_01: Enter the dates of holidays and vacation days for the years 2022 and 2023 in Hessen (Germany)
   - 02_02: Load the daily count data of the bicycle counting stations for the years 2022 and 2023
   - 02_03: Load the daily weather data at the bicycle counting stations for the years 2022 and 2023
      - 02_03_01: Load the excel files in R
      - 02_03_02: Analyze the influence of weather parameters on the count data to determine filter values for the cleaning of the data set
           - Boxplot: count data ~ average daily temperature
           - Boxplot: count data ~ amount of snow fall
           - Boxplot: count data ~ humidity
           - Boxplot: count data ~ wind speed
           - Boxplot: count data ~ amount of rain<br/><br/>

Files 03: Cluster analysis and validation
   - 03_01: Method 1: Cluster analysis using set day values for each counting station:
      Loop through the data and determine the highest number of counting stations which have the same dates of the count data<br/><br/>
   - 03_02: Method 2: Cluster analysis using the whole count data for each counting station
     - 03_02_01: Filter the data and execute the cluster analysis
        1. Filter the data:
           - remove NA-Values
           - remove days in holidays and vacation periods
           - remove days with average temperatues under 10 °C and with snowfall
           - remove days with unexpected high or low counts with the method "adjusted boxplot"
           - remove counting station with a median of unter 10 over the whole data or with a median of 0 on one day of the week
           - remove counting stations which doesnt have more than 8 count data on each day of the week
        2. Determine median weekly flow patterns for each counting station with the median of the daily counts for each day of the week
        3. Analyze the weekly flow patterns for the counting stations with the single linkage cluster analysis. Calculate euclidean distance and build groups of similar weekly flow patterns.
        4. Second filtering of the data: 
           - remove counting stations with an euclidean distance over 0.01 
           - remove counting stations with an euclidean distance of over 0.01 and unusueal weekly flow patterns
        5. Hierarchical cluster analysiés:
           - perform different types of hierarchical cluster analyses
           - plot the results of the cluster analyses in dendrograms and scree plots to determine the ideal number of cluster groups
        6. K-means clustering:
           - perform kmeans clustering with 1000 iterations and save the result with the highest average silhouette width (parameter for the stability of the cluster distribution)
           - save and plot the values of the weekly flow patterns for all methods of cluster analyses and the k-means clustering
           - plot the weekly flow patterns of the members of each cluster group
           - save results in excel file for further analyses in QGIS 
        7. other analyses:
           - analyze weekly flow patterns with low silhouette score and decide to remove them from analysis or not
           - plot number of counting stations per minimal amount of count data per day of the week<br/><br/>
     - 03_02_02: Validation of the results of the cluster analysis:
       - calculate  Rand-index, silhouette width and statistical parameters of the typical weekly flow patterns
       - plot the silhouette widths of the cluster result<br/><br/>
     - 03_02_02: Test the stability of the results of the cluster analysis:
       - repeat 100 times splitting the data in half and performing the whole cluster analysis with the calculation of the typical weekly flow patterns
       - save all results in one table
       - plot all results and determine the variance of the results to evaluete the stability and quality of the result of the cluster analysis<br/><br/>     

Folder "Spatial analysis"

File 01: Spatial analysis of the distribution of the counting stations to the cluster groups with typical weekly flow pattern
   - load the data, remove counting stations which was not grouped and general settings like the plot colors
   - analyze the relation of the cluster grouos with different spatial parameters and plot the results:
      - cluster group ~ RegioStaRGem5
      - cluster group ~ distance to closest secondary school      
      - cluster group ~ distance to closest river
      - cluster group ~ type of way
      - cluster group ~ population density in surrounding 1 km raster cell
      - cluster group ~ distance to urban area
      - cluster group ~ net category of bicycle way
     
