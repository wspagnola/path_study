# Path Study

## This is a study of smoking cessation based on the Population Assessment of Tobacco and Health survey.



## Notes
I originally did this project as part of my internship.   I made some changes from my original data-processing steps.  I also not some different strategies I would have used given what I know now about data processing. 

#### Data Processing
1. I worked on each wave individually and then merged the files.  I think I might have appended the files row=wise now and then pivoted the data longer. This might make the processing easier.  However, I'm not sure how consistent the columns are.  Instead of using the same variable for each wave, the column names in each dataset generally indicate the wave.  It seems as though the data is more designed to work in a wide format, and researchers are encouraged to merge the files to create a master data set in which each row is a study participant and most columns represents a given observation during a given wave (with the exception of some variables that should stay consistent across all three waves). 
2. I originally used the RData files instead of the tab-delimited files.  The RData files may have contained character data whereas the tab-delimited files were numerically-coded.  I rewrote the data-cleaning scripts to recode the numerically-coded data as factors.