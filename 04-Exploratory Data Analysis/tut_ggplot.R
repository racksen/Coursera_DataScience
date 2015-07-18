#' ggplot = an implementation of Grammer on Graphics by "Leyland Wilkinson" and implemented by "Hadley Wickham"
#' statiscal graphic system to visualize the data into 
#' --Aesthetic attributes(color, shape, size) of Geometric objects(points, lines, bars)  
#' Basic components are data frame, asthetic mappings, geom, facets, stats, scales and coordinates
#' ggplot following "Artistic Pallete" model, i.e,
#' --Plot the data
#' --Overlay summary
#' --Metadata and annotation

# Basic plotting
library(ggplot2)
g <- ggplot(mpg,aes(displ,hwy))
print(g) # will throw error bcox it doesn't know how to draw
p <- g + geom_point()
print(p)
g + geom_point() # auto plotting

g + geom_point() + geom_smooth() # smooth

g + geom_point(aes(color=drv), size=4, alpha=1/2) + geom_smooth(method="lm") # smooth by regression

# adding vertical facets layer
g + geom_point() + geom_smooth(method="lm") + facet_grid(.~drv)

# Modifying aesthetics
g + geom_point(aes(color=drv)) + geom_smooth(method="lm") + facet_grid(.~drv)

# Modifying labels
g + geom_point(aes(color=drv), size=4, alpha=1/2) + geom_smooth(method="lm") + labs(title= "Engine Size vs Mileage") + labs(x="Engine Size", y="Mileage")

# Modifying smooth
g + geom_point(aes(color=drv), size=4, alpha=1/2) + geom_smooth(method="lm", size=3, linetype=3, se=FALSE) + labs(title= "Engine Size vs Mileage") + labs(x="Engine Size", y="Mileage")

# Changing the theme
g + geom_point(aes(color=drv), size=4, alpha=1/2) + geom_smooth(method="lm") + labs(title= "Engine Size vs Mileage") + labs(x="Engine Size", y="Mileage") + theme_bw(base_family = "Ariel")

#axis limits comparison 
testdata <- data.frame(x=1:100, y=rnorm(100))
testdata[50,2] <-100 # set outliers
g <- ggplot(testdata,aes(x=x,y=y))
g+ geom_line()
g+ geom_line() + coord_cartesian(ylim= c(3,-3)) # you can see that the outlier included



