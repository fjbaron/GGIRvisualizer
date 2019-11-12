## Overview

This is a shiny app to visualize the variables computed with [GGIR](https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html),
 and how they are affected by the cutpoints and duration of the bouts definition.
 
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


## Remarks 
This code depend on a lot of libraries but hopefully they will be installed when you execute the shiny
app for the first time. Please try to keep them updated.

If you need help, please contact baron@uma.es. I'm happy to know if the code doesn't work in your system and why it happens.



