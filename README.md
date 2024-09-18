# R package pickmdl

The package relies on the R package 
[RJDemetra](https://CRAN.R-project.org/package=RJDemetra)
and provides additional functionality used in the production of statistics at Statistics Norway.

-----------

## Installation

Since *pickmdl* depends on *RJDemetra*, refer to the 
[installation instructions](https://github.com/rjdverse/rjdemetra/blob/main/README.md#Installation) 
on its GitHub page and note the availability of 
[the installation manual](https://github.com/jdemetra/rjdemetra/wiki/Installation-manual).

Usual installation from GitHub:
```r
# install.packages("devtools")
devtools::install_github("statisticsnorway/pickmdl")
```
If you know that the dependencies listed under *Imports* and *Depends* in the 
[DESCRIPTION file](https://github.com/statisticsnorway/ssb-pickmdl/blob/main/DESCRIPTION)
are already installed, an alternative is:
```r
devtools::install_github("statisticsnorway/pickmdl", dependencies = FALSE)
```

-----------


# Functionality

### PICKMDL specification

The x13 function in RJDemetra can be run as usual (automdl) or with a PICKMDL specification. 

* PICKMDL is an X-12-ARIMA procedure not yet available in RJDemetra.
* See https://github.com/jdemetra/jdemetra-app/issues/504

### Partial concurrent adjustment

The package has also possibilities for partial concurrent adjustment. 

* The ARIMA model, outliers and filters can be identified at a certain date and then held fixed (with a new outlier-span).
 
### Multiple series specifications
Specifications for multiple series can be managed through parameter settings in a table.

* The table may, for example, be stored as a CSV file. 
* See the function `x13_text_frame()`.

### The corona period
Handling the corona period according to 
[Statistics Norway's recommendation](https://github.com/statisticsnorway/ssb-seasonaladjustment-corona) 
can be done easily.

* By specifying the parameter `corona_outliers` as `TRUE` or `"SSB"`.



### Norwegian calendar regressors
A function for constructing user-defined calendar regressors is included.
 
* See the function `konstruksjon()`.


-----------

# Detailed documentation

More detailed documentation can be found at the [reference site for pickmdl](https://statisticsnorway.github.io/ssb-pickmdl/reference/index.html)
