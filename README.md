# Sailing Orders DFN 
**A bookdown project to produce Sailing Orders with standardized output for the surveys carried out on board the RV Dr. Fridtjof Nansen.**



## Installation of the desktop version

To be able to just build the gitbook or the static output (.docx/pdf) the code requires [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/). Install this software on your computer following the instructions on the respective webpages. Open RStudio and install the [bookdown](https://bookdown.org/) package:

```{r eval = FALSE}
install.packages("bookdown")
```

### Running the app from your hard drive

Click "Clone or download" -> "Download ZIP". Find the zip file (typically in your Downloads folder) and extract it to a desired location. Open the SailingOrdersDFN.Rproj file in RStudio, open the index.Rmd (look for it in the Files tab of the Viewer Panel in the bottom right of your R-Studio default installation panel view options) and run the first code chunk (name of code chunk 'setup'). Restart your R-Studio, reopen the SurveyReport.Rproj file and then select "bookdown::gitbook" located under the Build tab (click and expand the arrow next to the Build Book button). Running the first index.Rmd chunk from inside R-Studio for the first time **automatically installs and loads** packages used by the code. If you encounter installation problems, please read the error messages carefully. If you cannot solve these errors by installing the required packages manually, please contact the code maintainer.

## Components

The code contains 2 active .Rmd files the order of which for the knitting process is index.Rmd followed by 01-SailingOrder.Rmd. If an underscore precedes a numbered Rmd, that Rmd is silenced and not used in the knitting process. A lot of information/comments is currently in the different Rmds.

## Dependencies

The necessary packages for executing the code are listed in the index.Rmd