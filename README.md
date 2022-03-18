
# Shortest Path Example

This tutorial demonstrates how to determine the shortest distance between 
points within a system, in this case, acoustic telemetry receivers used to 
track fish movements. However, it could be used for other situations where
you would want to determine the shortest distance between points within a confined
spatial shape. 


# Example Study Site 
I chose to use Big Sissabagama lake located in northwest Wisconsin, USA as the 
example study system.

![](./Plots/big_sissabagama_lake.png)

This is the lake I grew up fishing on and for the sake of this tutorial, 
we will hypothetically have deployed 
an acoustic telemetry array within the lake to study walleye movements. 

![](./Plots/big_sissabagama_lake_receivers.png)

From this plot we can use functions from the [{sf}](https://r-spatial.github.io/sf/)
package to first create linestrings between each receiver and 
secondly to determine the shortest distance between each receiver. 


![](./Plots/big_sissabagama_lake_cost_dist.png)

The sf object created can then be kept together or ripped apart 
to determine the distance or path a fish could swim within the system along with
a whole host of other potential implications (e.g. interpolated paths).

Credit: R. Lennox, PhD for the original ideas around this script.  
