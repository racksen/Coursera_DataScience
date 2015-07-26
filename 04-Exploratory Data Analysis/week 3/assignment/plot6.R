#1. load required libraries
library(dplyr)
library(ggplot2)

#2. define data preparation function
init_datasets <- function (){
  #1. get the EPA data zip file
  zipFileLoc <- "./data/exdata-data-NEI_data.zip"
  
  if(!file.exists(zipFileLoc)){
    zipUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    tempZipFile <- tempfile();
    download.file(zipUrl,tempZipFile)
    zipFile <- unzip(tempZipFile)
    unlink(tempZipFile)
  } else {
    zipFile <- unzip(zipFileLoc)
  }
  
  #2. Read NEI and SCC data file if its not initialize before
  if(!exists("SCC")){
    SCC <- readRDS(if(zipFile[1]=="./Source_Classification_Code.rds") zipFile[1] else zipFile[2]) 
  }
  if(!exists("NEI")){
    NEI <- readRDS(if(zipFile[2]=="./summarySCC_PM25.rds") zipFile[2] else zipFile[1]) 
  }
  datasets <- list("SCC" = SCC, "NEI" = NEI)
}

#3. get the initialized datasets
if(!exists("dfEpa"))
  dfEpa <- init_datasets()


#4. Filter Combustion by only coal records from SCC
dfEpa$SCC_MotorVehicles <- filter(dfEpa$SCC,  grepl('Vehicles', EI.Sector))
dfEpa$SCC_MotorVehicles.SCCIds <- as.character(dfEpa$SCC_MotorVehicles$SCC)

#5. Fetch only Baltimore and LA Counties NEI data and mutate a new column to identify the county
dsNEI_byTwoCounties <- dfEpa$NEI %>% 
  filter(fips %in% c("24510","06037")) %>% 
  rowwise() %>% 
  mutate(county = (if(fips == "24510") "Baltimore City" else "Los Angeles County"))

#6. Summarize the Emissions by year for Baltimore city  
dsNEI_forMotorVehiclesByTwoCounties <- dsNEI_byTwoCounties %>% 
  filter(SCC %in% dfEpa$SCC_MotorVehicles.SCCIds )  %>%
  select(county,year,Emissions) %>% 
  group_by(county,year) %>% 
  summarise(total=sum(Emissions));

#7. set the par to compare two counties
par(mfrow= c(1,2))

#8. draw ggplot
g <- ggplot(dsNEI_forMotorVehiclesByTwoCounties,aes(year,total))
g + geom_point(aes(color=county), size=4, alpha=1/2) + 
  geom_smooth(method="lm") + facet_grid(.~county) +
  labs(title= "PM(2.5) Emissions in Baltimore, MD and Los Angeles, CA  due to Motor Vehicles") + 
  labs(x="Year", y="Total Emissions (Tones)")

#9. generate png file and switch off the png device immediately
dev.copy(png, file="plot6.png", width=900, height=580)
dev.off()


