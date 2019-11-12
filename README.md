## Overview

This is a shiny app to visualize the variables computed with GGIR,
 and how they are affected by the cutpoints and duration of the bouts definition.
 
## Usage

You need to put the files app.R, executeMeFist.R and parameters.R where GGIR has 
 put the meta and results folders. If you prefer to put in in another place you can open
the file parameters.R and modify the variable that contains the folder with GGIR computations 

baseEpoch="." 
modify it to your chosen folder:
baseEpoch="/my/GGIR/computation/folder/..."


GGIR is very configurable, so if you are not using his default parameters, you need to
 modify that correspondingly.
 
 
This code depend on a lot of libraries but hopefully they will be installed when you execute the shiny
app for the first time.
 
If you need help, please contact baron@uma.es



