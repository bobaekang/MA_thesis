# Bobae's MA Thesis materials - `thesis_materials`

Author: Bobae Kang @bobaekang  
Advisor: Dr. Benjamin Soltoff @bensoltoff  
  
## Description  
This directory stores materials for data collection, preparation, analysis, and visualization.   

### sub-directories 
* `data`: stores data generated using the R scripts.  
* `data_predownload`: stores all pre-downloaded materials, including demographic and weather data.  
* `images`: stores images generated using the R scripts.  
* `rawdata`: stores data downloaded using the R scripts.  
  
### R scripts
* `thesis00_download.R`: R script to download Divvy and CTA data.
* `thesis01_prepare1.R`: R script to prepare/tidy data for analysis; combines Divvy and CTA data and adds proximity variables.
* `thesis02_prepare2.R`: R script to prepare/tidy data for analysis; adds geographic-demographic variables.
* `thesis03_sample_generation.R`: R script to generate the sample data and add potential muti-modality variable.
* `thesis04_plots.R`: R script to generate some exploratory plots of the data.
* `thesis05_tables.R`: R script to get descriptive statistics for tables.
* `thesis06_lab.R`: R script to serve as an laboratory; fits models and inspects the estiamted results.