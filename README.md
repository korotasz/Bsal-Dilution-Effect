# Chapter-2-Analyses

## Description
This chapter of my dissertation leveraged publicly available data to test the dilution effect hypothesis in the amphibian-Bsal system. This GitHub repository is where all files included in data collection, processing, and analysis can be found. Included R files correspond with data cleaning and data analyses for this chapter. Details on program and package versions can be found below in the [Project Requirements](https://github.com/korotasz/Chapter-2-Analyses/edit/main/README.md#Project-Requirements) section of this document. 

## Getting Started
### Project Requirements
**Requirements** to run the code for this project include:
- R version 4.3.1
- R Studio version 2023.06.1+524 'Mountain Hydrangea'
- ***renv*** R package version 1.0.2
    - This package was used for version control of packages used in this project. More information about ***renv*** can be found [here](https://rstudio.github.io/renv/articles/renv.html). 
    - All other info regarding package versions can be found in the [renv.lock](https://github.com/korotasz/Chapter-2-Analyses/blob/main/renv.lock) file.

### Using the Code
![fork](https://cdn.ttgtmedia.com/rms/onlineimages/cdo-git_clone_vs_fork-f.png)
1. Please **fork** this repo if you intend to use or modify any of the code to avoid future merge conflicts. GitHub has an excellent example of [how to fork repositories](https://docs.github.com/en/get-started/quickstart/fork-a-repo#forking-a-repository) from the browser. Alternatively, the code to do so from **Command Prompt** can be copied below.
   ```
   git fork https://github.com/korotasz/Chapter-2-Analyses/
   ```

2. After you have forked this repository, you may then **clone** the forked repository to your local system. Cloning forked repositories may be done [from Git Bash](https://docs.github.com/en/get-started/quickstart/fork-a-repo#cloning-your-forked-repository) or from **Command Prompt**. In either case, the syntax is the same for cloning repositories:
   ```
   git clone https://github.com/GitHub_username/forked-repo-name
   ```

3. In R, navigate to the folder containing the cloned repository and open the [Chapter-2-Analyses.Rproj](https://github.com/korotasz/Chapter-2-Analyses/blob/main/Chapter-2-Analyses.Rproj) file.
   - This project file will organize the R environment so that all relevant scripts, files, and sub-folders are structured how they need to be to efficiently run the R scripts. Click [here](https://r4ds.had.co.nz/workflow-projects.html#rstudio-projects) to learn more about R projects.

4. Within the opened project, open the [dataAnalysis_DEH](https://github.com/korotasz/Chapter-2-Analyses/blob/main/dataAnalysis_DEH.R) R script.

5. **Before running any part of the script**, make sure the ***renv*** package is installed and loaded:
   ```
   install.packages('renv', version = '1.0.2')
   library(renv)
   ```
   
6. You will then need to call ```renv::restore()``` to reinstall the specific package versions used in this project.

7. Once the required package versions have been installed, you are ready to run the R script. Happy coding!

## Maintainers
[@korotasz](https://github.com/korotasz)

## Contributing
The purpose of this repository is to ensure code reproducibility for this project. While feedback is appreciated, I ask that you refrain from submitting pull requests.

### Collaborators
This project exists thanks to everyone who helped in some way. Your contributions are appreciated.
[Jason R. Rohr](https://scholar.google.com/citations?user=yaRksUAAAAAJ&hl=en)
[Jaime Bosch](https://scholar.google.com/citations?user=t5frSGQAAAAJ&hl=en)
[An Martel](https://biblio.ugent.be/person/F5F50C8C-F0ED-11E1-A9DE-61C894A0A6B4)
[Frank Pasmans](https://biblio.ugent.be/person/F573FD86-F0ED-11E1-A9DE-61C894A0A6B4)

## License
[Creative Commons Zero v1.0 Universal](https://github.com/korotasz/Chapter-2-Analyses/blob/main/LICENSE)
