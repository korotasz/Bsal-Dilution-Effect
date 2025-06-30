# Chapter-2-Analyses

## Description
This chapter of my dissertation leveraged publicly available data, supplemented with unpublished records provided by collaborators, to test the dilution effect hypothesis in the amphibian-Bsal system. This GitHub repository is where all files included in data collection, processing, and analysis can be found. Included R files correspond with data cleaning and data analyses for this chapter. Details on program and package versions can be found below in the **Project Requirements** section of this document. 

## Project Requirements 
   This project primarily uses R within the RStudio IDE. Version control within the R project is maintained using the *renv* package. The versions of R and *renv* are listed below, with links to their respective sources. These versions are **absolutely necessary** to effectively run both the *dataCleaning.R* and *dataAnalysis_DEH.R* scripts.
   
   <a href="https://cran.r-project.org/bin/windows/base/old/4.3.3/"><img src="https://img.shields.io/badge/_R-v.4.3.3-%23fbfbfb?style=plastic&logo=R&logoColor=%23e9e9e9&logoSize=auto&labelColor=%23246ABE"/></a>
   <a href="https://rstudio.github.io/renv/articles/renv.html"><img src="https://img.shields.io/badge/renv-v.1.0.5-%23fbfbfb?style=plastic&logoSize=auto&labelColor=%23a8deb5"/></a>

   All other info regarding R package versions can be found in the [renv.lock](https://github.com/korotasz/Chapter-2-Analyses/blob/main/renv.lock) file.

  Python was used to obtain weather data from the [GLDAS dataset](https://disc.gsfc.nasa.gov/datasets/GLDAS_CLSM025_DA1_D_2.2/summary?keywords=GLDAS%20Catchment%20Land%20Surface%20Model%20L4%20daily%200.25%20x%200.25%20degree%20GRACE-DA1%20V2.2%20(GLDAS_CLSM025_DA1_D%202.2)) available through [NASA EarthData](https://www.earthdata.nasa.gov/), and QGIS was used to process the .nc4 files. The weather data has already been integrated into our dataset, but if you wish to run any of the .py scripts in the [weatherSampling folder](https://github.com/korotasz/Chapter-2-Analyses/blob/main/01_dataCleaning/weatherSampling), you will need the following versions of Python and QGIS:
  
  <a href="https://www.python.org/downloads/"><img src="https://img.shields.io/badge/Python-v.3.12.0-%234584b6?style=plastic&logo=Python&logoColor=%23ffde57&logoSize=auto&labelColor=%234584b6&color=%23fbfbfb"/></a>
  <a href="https://ftp.osuosl.org/pub/osgeo/download/qgis/windows/"><img src="https://img.shields.io/badge/QGIS-v.3.26.2--1-%23fbfbfb?style=plastic&logo=Qgis&logoColor=%23f18d36&logoSize=auto&labelColor=%23589632&color=%23fbfbfb"/></a> 

## Using the Code
![fork](https://ubc-library-rc.github.io/intro-git/content/figures/git-collaboration_2.png)
<sup>(image credit: UBC Library Research Commons)</sup>
   1. Please **fork** this repo if you intend to use or modify any of the code to avoid future merge conflicts. Forking will allow you to make changes to your own copy of the repository without affecting any of the files in this repository. [This github.io page](https://ubc-library-rc.github.io/intro-git/content/05_collab_on_github.html#:~:text=top%20right%20corner.-,Clone%20the%20fork%20to%20your%20own%20computer,with%20your%20preferred%20text%20editor.) has an excellent section explaining both the 'why' and 'how' of forking repositories.
   
   2. After you have forked this repository, you may then **clone** the forked repository to your local system. Cloning forked repositories may be done [from Git Bash](https://docs.github.com/en/get-started/quickstart/fork-a-repo#cloning-your-forked-repository) or from **Command Prompt**. In either case, the syntax is the same for cloning repositories:
      ```
      git clone https://github.com/GitHub_username/forked-repo-name
      ```
   
   3. In R, navigate to the folder containing the cloned repository and open the [Chapter-2-Analyses.Rproj](https://github.com/korotasz/Chapter-2-Analyses/blob/main/Chapter-2-Analyses.Rproj) file.
      - This project file will organize the R environment so that all relevant scripts, files, and sub-folders are structured how they need to be to efficiently run the R scripts. Click [here](https://r4ds.had.co.nz/workflow-projects.html#rstudio-projects) to learn more about R projects.
   
   4. Within the opened project, open the [dataAnalysis_DEH](https://github.com/korotasz/Chapter-2-Analyses/blob/main/02_dataAnalyses/dataAnalysis_DEH.R) R script.
   
   5. **Before running any part of the script**, make sure the ***renv*** package is installed and loaded:
      ```
      install.packages('renv', version = '1.0.5')
      require(renv)
      ```
      
   6. You will then need to call ```renv::restore()``` to reinstall the specific package versions used in this project.
   
   7. Once the required package versions have been installed, you are ready to run the R script. Happy coding! 🎊

**Notes:**
- All specific location data (i.e., Lat and Lon) have been removed from .csv files out of consideration for sensitive populations. All data may be available upon request, with permission from the co-authors.
  
- The [data cleaning file](https://github.com/korotasz/Chapter-2-Analyses/blob/main/01_dataCleaning/dataCleaning.R) may still be run from line 1495 onward by loading the [dataCleaning.Rdata](https://github.com/korotasz/CHapter-2-Analyses/blob/main/01_dataCleaning/dataCleaning.RData) file in the R Global Environment and loading the required packages (lines 1-43).


## Maintainers
[@korotasz](https://github.com/korotasz)

## Contributing
The purpose of this repository is to ensure code reproducibility for this project. While feedback is appreciated, I ask that you refrain from submitting pull requests.

## Collaborators
Collaborators on this project include [Philipp Böning](https://www.uni-trier.de/universitaet/fachbereiche-faecher/fachbereich-vi/faecher/biogeographie/profile/boening-philipp), [Jaime Bosch](https://scholar.google.com/citations?user=t5frSGQAAAAJ&hl=en), [Stefan Lötters](https://www.loetterslab.de/),  [An Martel](https://biblio.ugent.be/person/F5F50C8C-F0ED-11E1-A9DE-61C894A0A6B4), [Frank Pasmans](https://biblio.ugent.be/person/F573FD86-F0ED-11E1-A9DE-61C894A0A6B4), [Michael Veith](https://www.researchgate.net/profile/Michael-Veith-2/28), and [Jason R. Rohr](https://scholar.google.com/citations?user=yaRksUAAAAAJ&hl=en). This project exists thanks to their contributions.





## License
[Creative Commons Zero v1.0 Universal](https://github.com/korotasz/Chapter-2-Analyses/blob/main/LICENSE)
