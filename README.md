# R package pickmdl

* The x13 function in RJDemetra can be run as usual (automdl) or with a PICKMDL specification. 
  - PICKMDL is an X-12-ARIMA procedure not yet available in RJDemetra.
  - See https://github.com/jdemetra/jdemetra-app/issues/504
* The package has also possibilities for partial concurrent adjustment. 
  - The ARIMA model, outliers and filters can be identified at a certain date and then held fixed (with a new outlier-span).
  
  
More detailed documentation can be found at the [reference site for pickmdl](https://statisticsnorway.github.io/ssb-pickmdl/reference/index.html)
