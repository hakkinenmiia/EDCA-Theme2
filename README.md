# EDCA-Theme2
EDCA course at WUR, theme 2 group project 
#### THEME 2 PROJECT Data analyis 
#################################
rm(list = ls())

#potentially useful libraries to download:
library(readr)
library(reshape)
library(ggmap)
library(ggplot2)
library(oce)
library(effects)
library(stats)
library(stats4)
library(vegan)
library(car)
library(ggpubr)
library(reshape2)
library(foreign)
library(RColorBrewer)
library(sp)
library(rgdal)
library(maptools)
library(gstat)

###### DATA ANALYSIS 
#DATA IMPORT 
##Test for soil data - just fooling around and trying things!
# read data file:
#Data files: soiltransect, topsoilvariability, lake water and lake sediment 
soil = read.table("soiltransect_R.csv", sep=";", header=T)
#automatic data import 
topsoil <- topsoilvariability_R
names(topsoil)

##Sorting out data 
#subsets
west <- subset(topsoil, Area==c("W"))
east <- subset(topsoil, Area==c("E"))

#RQ 1 

# some info of data 
dim(soil) #dimentions 80 rown in 15 columns
class(soil) #type of data file - data frame 
names(soil) #column names 
summary(soil) #basic statistics of each column

#Basic statistics 
mean()
sd()

boxplot(topsoil$`Cu (mg/kg) 0.43 M HNO3`~topsoil$Area, ylab="Cu (mg/kg) 0.43 HNO3")

#Normality test to prove that our data is parametric: 
s <- glm(y~x)
shapiro.test(resid(s)) # parametric when p-value > 0.05

# test for homogeniety of variance:
bartlett.test(y~x) 
leveneTest(y~x) # parametric when p-value > 0.05

#visual check: 
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))

#t-test between areas
#--> use for lake sediment, water samples, soil samples to find out significant differences 

#the codes below are from another script modify to right dataset 
#t-test
t.test(ns_high$ability~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica"))
#2-way ANOVA   
#with interactions
res.aov2 <- aov(nd$ability~nd$treatment + nd$concentration + nd$treatment * nd$concentration)
plot(res.aov2)

summary(res.aov2)

#Q2
# loading the required packages
library(ggplot2)
library(ggmap)
library(qmplot)

#automatic data import 
topsoil <- topsoilvariability_R
names(topsoil)

top.var <- subset(topsoil, `Sample type` == c("topsoil site"))

##Mapping the measuring points 
#get map
#the map didn't work without API key from google maps

map <- get_map(location = c(lon = mean(top.var$Long), lat = mean(top.var$Lat)), zoom = 15,
               maptype = "satellite", scale = 2) 

area <- ggmap(map)  + geom_point(data=top.var, aes(top.var$Long, top.var$Lat), color="red")
area

#These are copied and modified from the lecture script

# define gstat object and compute variogram:
#id=identifier of the new variable
#formula=defines the dependent variable as a linear model of independent varaibles
#for ordinary and simple kirging formula is z~1
#for universal kriging where z is linearly dependen on x and y, use formula z~x+y
g = gstat(id = c("Cu (mg/kg) 0.43 M HNO3"), formula =top.var$`Cu (mg/kg) 0.43 M HNO3`~1, data=top.var)
#this line works but the next command vg doesn't work so maybe something is wrong here,
#I also found odd that g only gets one value. 
?gstat

vg = variogram(g) #<-- getting error message here and don't know how to proceed 

vg = variogram(g, width = 20000, cutoff = 600000)
plot(vg, plot.numbers = TRUE)

vgm = vgm(nugget=20000, psill=80000, range=2E5, model="Exp")
?vgm
plot(vg, vgm)



###################
##SEMIVARIOGRAM 
#loading the required packages
library(ggplot2)
library(ggmap)
library(qmplot)

#data import and views

topsoil <- read.csv("topsoilvariability_R_GH.csv", sep=";", header=TRUE)
names(topsoil)
top.var <- subset(topsoil, `Sample_type` == c("topsoil_site"))
dim(top.var)
summary(top.var)
hist(top.var$Cu,col='Lightblue')  # looks pretty symmetric so no need for transformation
hist(top.var$Zn,col='Orange')  # looks pretty symmetric so no need for transformation

#data subsets into west and east
west <- subset(top.var, Area==c("W"))
east <- subset(top.var, Area==c("E"))


#geographic projection

library(rgdal)
library(sp)
library(gstat)
library(raster)

coordinates(top.var) =~Long+Lat
proj4string(top.var)
?proj4string

crs(top.var)
crs(top.var) <- CRS("+proj=longlat +datum=WGS84")
crs(top.var)
str(top.var)
spplot(top.var, zcol="Cu", scales=list(draw=TRUE))

?spTransform

top.var.nlgrid <- spTransform(top.var, CRS("+init=epsg:28992"))
str(top.var.nlgrid)
spplot(top.var.nlgrid, zcol="Cu", scales=list(draw=TRUE))
str(top.var.nlgrid)
summary(top.var.nlgrid@coords)


#variography

gCu = gstat(id = c("Cu"), formula = Cu~1, data = top.var.nlgrid)
vgCu = variogram(gCu, width=20,cutoff=20000)
plot(vgCu, plot.nu=T)
?variogram

vg = variogram(g, width = 20000, cutoff = 600000)
plot(vg, plot.numbers = TRUE)

vgm = vgm(nugget=20000, psill=80000, range=2E5, model="Exp")
plot(vg, vgm)

## West area, copper
coordinates(west) =~Long+Lat
proj4string(west)
crs(west)
crs(west) <- CRS("+proj=longlat +datum=WGS84")
crs(west)
str(west)
spplot(west, zcol="Cu", scales=list(draw=TRUE))

west.nlgrid <- spTransform(west, CRS("+init=epsg:28992"))
str(west.nlgrid)
spplot(west.nlgrid, zcol="Cu", scales=list(draw=TRUE))
str(west.nlgrid)
summary(west.nlgrid@coords)

#variogram
gwCu = gstat(id = c("Cu"), formula = Cu~1, data = west.nlgrid)
vgwCu = variogram(gwCu, width=20,cutoff=2000)
plot(vgwCu, plot.nu=TRUE)


vgm_wcu = vgm(nugget=5, psill=25, range=100, model="Exp")
plot(vgwCu, vgm_wcu)

vgm2 = fit.variogram(vgwCu, vgm_wcu)
plot(vgwCu, vgm2)
var(west$Cu) #24.86629 <- lower than in graph 

west.nlgrid@data

### East area, copper 
coordinates(east) =~Long+Lat
proj4string(east)
crs(east)
crs(east) <- CRS("+proj=longlat +datum=WGS84")
crs(east)
str(east)
spplot(east, zcol="Cu", scales=list(draw=TRUE))

east.nlgrid <- spTransform(east, CRS("+init=epsg:28992"))
str(east.nlgrid)
spplot(east.nlgrid, zcol="Cu", scales=list(draw=TRUE))
str(east.nlgrid)
summary(east.nlgrid@coords)

geCu = gstat(id = c("Cu"), formula = Cu~1, data = east.nlgrid)
vgeCu = variogram(geCu, width=20,cutoff=2000)
plot(vgeCu, plot.nu=TRUE)


vgm_ecu = vgm(nugget=30, psill=120, range=100, model="Exp")
plot(vgeCu, vgm_ecu)

vgm3 = fit.variogram(vgeCu, vgm_ecu)
plot(vgeCu, vgm3)
#parameter estimations: nugget=60, sill=?, range=?

var(east$Cu) #120.223 

# Combine the semivariograms into two 

plot(gamma~dist, vgeCu, ylim = c(0, 1.05*max(vgeCu$gamma)),col='red', ylab =
       'semivariance', xlab = 'distance')
lines(variogramLine(vgm3,350), col='red')

points(gamma~dist, vgwCu, col='blue')
lines(variogramLine(vgm2, 1500), col='blue')

############################################
### West area, zinc
gwZn = gstat(id = c("Zn"), formula = Zn~1, data = west.nlgrid)
vgwZn = variogram(gwZn, width=20,cutoff=5000)
plot(vgwZn, plot.nu=TRUE)


vgm_wzn = vgm(nugget=100, psill=400, range=100, model="Exp")
plot(vgwZn, vgm_wzn)

vgm4 = fit.variogram(vgwZn, vgm_wzn)
plot(vgwZn, vgm4)
#parameter estimations: nugget=100, sill=500, range=300

var(west.nlgrid$Zn) #513.866 <-- same/lower as sill in the graph 

### East area, zinc
geZn = gstat(id = c("Zn"), formula = Zn~1, data = east.nlgrid)
vgeZn = variogram(geZn, width=20,cutoff=2000)
plot(vgeZn, plot.nu=TRUE)

vgm_ezn = vgm(nugget=600, psill=1500, range=150, model="Exp")
plot(vgeZn, vgm_ezn)

vgm5 = fit.variogram(vgeZn, vgm_ezn)
plot(vgeZn, vgm5)
#parameter estimations: nugget=100, sill=520, range=300

var(east.nlgrid$Zn) #513.8668 <-- lower as sill in the graph 

# Combine the semivariograms into two 

plot(gamma~dist, vgeZn, ylim = c(0, 1.05*max(vgeZn$gamma)),col='red', ylab =
       'semivariance', xlab = 'distance')
lines(variogramLine(vgm5,350), col='red')
points(gamma~dist, vgwZn, col='blue')
lines(variogramLine(vgm4, 1500), col='blue')
