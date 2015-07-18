library(ggplot2)

#' All about qplot
#' A handy ploting function to quickly explore the data with automatic default settings

str(mpg)
# Create a veriy simple plot using ggplot
qplot(displ,hwy,data=mpg, xlab = "displ(Engine Cylinders)", ylab = "hwy(Mileage)")

# Modifying asthetics attributes
qplot(displ,hwy,data=mpg, color=drv,
      xlab = "displ(Engine Cylinders)", 
      ylab = "hwy(Mileage)")

# adding geom
qplot(displ,hwy,data=mpg, color=drv, geom = c("point","smooth"),
      xlab = "displ(Engine Cylinders)", 
      ylab = "hwy(Mileage)")

# Facets setup - Multiple vertical panels for each factor
qplot(displ,hwy,data=mpg, facets = .~drv , color=drv, 
      xlab = "displ(Engine Cylinders)", 
      ylab = "hwy(Mileage)")

# Facets setup - Multiple horizontal panels for each factor
qplot(displ,hwy,data=mpg, facets = drv~. , color=drv, 
      xlab = "displ(Engine Cylinders)", 
      ylab = "hwy(Mileage)")


# histogram with one variable
qplot(hwy,data=mpg, fill=drv)
# histogram with horizontal facets by drv factor
qplot(hwy,data=mpg, fill=drv, facets = drv~., binwidth=2 )

