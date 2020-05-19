## Changing travel patterns in China during the early stages of the COVID-19 pandemic

**Authors:** Hamish Gibbs\*<sup>1</sup>, Yang Liu\*<sup>1</sup>, Carl AB Pearson<sup>1</sup>, Christopher I Jarvis<sup>1</sup>, Chris Grundy<sup>1</sup>, Billy Quilty<sup>1</sup>, Charlie Diamond<sup>1</sup>, LSHTM CMMID COVID-19 working group<sup>1</sup>, Rosalind M Eggo<sup>1</sup>  
\* authors contributed equally  
Affiliations:  
<sup>1</sup> Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine, Keppel Street, London. WC1E 7HT. UK  

Open respository of code supporting the analyses in the paper: *Changing travel patterns in China during the early stages of the COVID-19 pandemic*. Available on [medRxiv](https://www.medrxiv.org/content/10.1101/2020.05.14.20101824v1).

Analyses are conducted using a symmetrical matrix of movement flux data collected from the Qianxi Baidu [2020 movement map](https://qianxi.baidu.com/2020/). The full dataset is included in this repository at `data/china_prf_connectivity_0101_0301.rds`.

This research also relies on Covid-19 case count data and prefecture level hospital resource data from China, C. D. C. [Public Health Science Data Center](http://www.phsciencedata.cn/Share/), as well as prefecture level population data from [China Statistical Yearbook 2018](http://www.stats.gov.cn/tjsj/ndsj/2018/indexeh.htm). Covid-19 case count data, prefecture level hospital resource data, and prefecture level population data require applications for use. 

#### Software versions
R code is compatible with R ~3.6. 
Python code is compatible with Python ~3.7.

#### Dependencies
**R packages:**  
[ggplot2 3.3.0](https://ggplot2.tidyverse.org/)  
[dplyr 0.8.3](https://dplyr.tidyverse.org/)  
[readr 1.3.1](https://readr.tidyverse.org/)  
[ggrepel 0.8.2](https://github.com/slowkow/ggrepel)  
[cowplot 1.0.0](https://github.com/wilkelab/cowplot)  
[data.table 1.12.8](https://cran.r-project.org/web/packages/data.table/data.table.pdf)  
[cluster 2.1.0](https://cran.r-project.org/web/packages/cluster/cluster.pdf)  
[factoextra 1.0.7](https://cran.r-project.org/web/packages/factoextra/index.html)  
[mapview 2.6.11](https://r-spatial.github.io/mapview/)  
[magrittr 1.5](https://cran.r-project.org/web/packages/magrittr/index.html)  
[ggpubr 0.3.0](https://cran.r-project.org/web/packages/ggpubr/index.html)  
[viridis 0.5.1](https://cran.r-project.org/web/packages/viridis/viridis.pdf)  
[sf 0.9](https://cran.r-project.org/web/packages/sf/index.html)  

**Python packages:**  
[glob 10.7](https://docs.python.org/2/library/glob.html)  
[pandas 1.0.3](https://pandas.pydata.org/)  
[igraph 0.8.2](https://github.com/igraph/python-igraph)  
[leidenalg 0.8.0](https://pypi.org/project/leidenalg/)  
[re 3.8.3](https://docs.python.org/3/library/re.html)  
[numpy 1.19.0](https://github.com/numpy/numpy)  

#### Installation
Clone this repository and use Python or R to run the source files. See the file reference below for details on required datasets and expected outputs.

#### Overview
-`calc_Access2Care.R` calculates access to healthcare and healthcare pressure on the prefecture level.
-`calc_Connectivity_ByPopQ.R` contains stratified analyses of inter-prefecture travel patterns.  
-`extract_ts.R` extract origin or destination specific travel time\-series.
-`reduce2_ntile.R` reduces dimension of connectivity matrix from 366\*366 dimensions to n\*n based on population.  
-`rank_variability.R` compare the travel surge in Wuhan to the rest of mainland China before Lunar New Year.  
-`trajectory_clustering_analysis.R` cluster normalized outflow trajectories from Chinese prefectures.
-`create_cluster_piv.R` create a long version of clustering analysis results.  
-`create_cluster_sum.R` create a summary of clustering analysis results.  
-`leiden.py` compute daily prefecture communities.  
-`all_module_Q_breakdown.R` compute Q of all identified communities.  
-`community_sizes.R` compute size of all identified communities.  

#### Instructions for use:

After downloading this repository, load the datasets required to conduct each analysis. Please see the file reference below. 

#### Demo:
Because of the application requirement for some datasets used in this analysis, it is not possible to provide a full demonstration of the output of each file. Please see the expected output reference below. 

#### File reference:

###### calc_Access2Care.R
**Datasets required:**
Movement matrix, case count data, hospital location data. 
**Expected output:**
Plots of changes in traveler volume and timeseries box plots of changes in healthcare pressure.

###### calc_Connectivity_ByPopQ.R
**Datasets required:**
Movement matrix, population data.
**Expected output:**
Plot of in- and out-bound traveler volume by population quartile.

###### extract_ts.R
**Datasets required:**
Movement matrix.
**Expected output:**
Plot of in- or out-bound travel volume for a single origin or destination.

###### rank_variability.R
**Datasets required:**
Movement matrix, population data.
**Expected output:**
Plot of peak outflow deviation from 2019 outflow values for all prefectures.

###### trajectory_clustering_analysis.R
**Datasets required:**
Movement matrix.
**Expected output:**
Line plot and silhouette plot of clustered outflow trajectories for a single origin prefecture for a given number of clusters.

###### create_cluster_piv.R
**Datasets required:**
Movement matrix.
**Expected output:**
"Long" output from `trajectory_clustering_analysis.R`. 

###### create_cluster_sum.R
**Datasets required:**
Movement matrix, case count data, population data.
**Expected output:**
Summary of prefecture cluster labels, population, and date of first case arrival.

###### leiden.py
**Datasets required:**
Movement matrix.
**Expected output:**
Dataset of community labels over time computed using the Leiden algorithm. 

###### all_module_Q_breakdown.R
**Datasets required:**
Movement matrix.
**Expected output:**
Plots of community modularity over time. 

###### community_sizes.R
**Datasets required:**
Movement matrix.
**Expected output:**
Dataset of prefecture community sizes over time. 

