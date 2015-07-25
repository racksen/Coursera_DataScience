#1. load required libraries
library(dplyr)

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
if(!exists("epaDatasets"))
  epaDatasets <- init_datasets()

#4. Summarize the Emissions by year for the Baltimore City, Maryland (fips == 24510) 
dfEmissionByYear.MaryLand <- epaDatasets$NEI %>% 
  filter(fips==24510) %>%
  select(year,Emissions) %>% 
  group_by(year) %>% 
  summarise(total=sum(Emissions)/10^3);

#5. draw histogram to compare the emissions by each year
plot(dfEmissionByYear.MaryLand,pch=19, xlab = "Year", ylab =  "Total Emissions (in Thousands)", main="Yearly Total Emissions(Baltimore City, Maryland)")
lines(dfEmissionByYear.MaryLand$year,dfEmissionByYear.MaryLand$total,col="red")

#6. generate png file and switch off the png device immediately
dev.copy(png, file="plot2.png", width=580, height=480)
dev.off()


