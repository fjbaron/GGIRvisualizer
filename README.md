## Overview

This is a shiny app to visualize the variables computed with [GGIR](https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html),
 and how they are affected by the cutpoints and duration of the bouts definition.


## Demo
If you want to try a demo before installing, you can test it in this [shiny app](https://datahunter.es/shinyapps/ggirvisualizer/). There you can select an accelerometry file, and click in the left menu to:

 * Visualize where the differents WHATs (bed, physical activity,...) happens. You can select **multiples files** at the same time to compare.
 
 * Time series: You can examine and zoom inside the time series of Epochs computed by GGIR. You can see there in color the different activities that GGIR has computed, ore the ones defined by you (see below). You can see this if you have previously selected **just one file**.
 
 * Get databases of summaries and other exploratory plots for sets of accelerometry files selected.


 
## Usage

You need to put the files `app.R`, `executeMeFist.R` and `parameters.R` where GGIR has 
 put the meta and results folders. If you prefer to put in in another place you can open
the file parameters.R and modify the variable that contains the folder with GGIR computations 

```
baseEpoch="."
# modify it to your chosen folder:
rbaseEpoch="/my/GGIR/computation/folder/..."
```


GGIR is very configurable, so if you are not using his default parameters, you need to
 modify that correspondingly. For that open the file `parameters.R` and modify or include new variables:
 
### Before 
```r
 defineWhat=list(
  MVPA_10m=function(df) enmoOver   (df, limInf=100/1000, pctBouts=0.8, durBoutMin=dminutes(10)),
  INAC_20m=function(df) enmoUnder  (df, limSup=40/1000,  pctBouts=0.9, durBoutMin=dminutes(20))
)

 ```
### After
```r
defineWhat=list(
  MVPA_10m=function(df) enmoOver   (df, limInf=100/1000, pctBouts=0.8, durBoutMin=dminutes(10)),
  #I add a new variable here to measure: 
  # Intervals of 5+ minutes with 90% of the time with acceleretation over 200 mili-g
  myNewMVPA = function(df) enmoOver  (df, limInf=200/1000, pctBouts=0.9, durBoutMin=dminutes(5)),
  # Different cutpoint for inactivity, but I keep the same name.
  INAC_20m=function(df) enmoUnder  (df, limSup=30/1000,  pctBouts=0.9, durBoutMin=dminutes(20))
)
 ```


Once you are happy with the modifications, you need to run the script `executeMeFirst.R` to process GGIR files
with your new definitios, and after finishing, you are ready to run the shiny app `app.R`.



## Remarks 
This code depend on a lot of libraries but hopefully they will be installed when you execute the shiny
app for the first time. Please try to keep them updated.

If you need help, please contact baron@uma.es. I'm happy to know if the code doesn't work in your system and why it happens.



