# sinkrtools

# sinkrtools

sinkrtools provide a simple interface to the [sinkr](https://github.com/marchtaylor/sinkr) package.


### Requirements

+ [R v 3.2+](https://www.r-project.org/)
+ [rscripting](https://github.com/BigelowLab/rscripting)
+ [sinkr](https://github.com/marchtaylor/sinkr)
+ [R.matlab](https://cran.r-project.org/web/packages/R.matlab/index.html)


### Installation

```R
# install.packages(devtools) # if not installed already

library(devtools)

devtools::install_github("BigelowLab/rscripting")
devtools::install_github("BigelowLab/sinkrtools")
```

### Scripts

`Rscript --vanilla /path/to/dineof_matlab.Rscript --inputfile foo.mat --inputvar allChl --maskfile mask.mat --maskvar nans ...`

```
Usage:
dineof_matlab [--inputfile character] [--inputvar character] [--maskfile
     character] [--maskvar character] [--n_cores character] [--step character]
     [--output character]

Argument details follow
--inputfile character 
    input matlab filename 
    default:  
--inputvar character 
    name of the item in the input matlab file 
    default: first 
--maskfile character 
    input matlab mask filename 
    default:  
--maskvar character 
    name of the item in the input matlab mask file 
    default: nans 
--n_cores numeric 
    the number of cores for parallel processing, default = 1 
    default: 1 
--step numeric 
    if n_cores > 1 then divide into steps this big 
    default: 360 
--output character 
    filename for output matlab file 
    default: dineof.mat 
```