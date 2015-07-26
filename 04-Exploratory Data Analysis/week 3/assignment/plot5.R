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

#View(dfEpa$SCC)

#4. Filter Combustion by only coal records from SCC
dfEpa$SCC_MotorVehicles <- filter(dfEpa$SCC,  grepl('Vehicles', EI.Sector))
dfEpa$SCC_MotorVehicles.SCCIds <- as.character(dfEpa$SCC_MotorVehicles$SCC)

#5. Summarize the Emissions by year for Baltimore city  
dsNEI_OnlyByMotorVehicles <- dfEpa$NEI %>% 
  filter(fips==24510 & SCC %in% dfEpa$SCC_MotorVehicles.SCCIds )  %>%
  select(year,Emissions) %>% 
  group_by(year) %>% 
  summarise(total=sum(Emissions));

#6. set the par 
par(mfrow= c(1,1))

#7. draw ggplot
g <- ggplot(dsNEI_OnlyByMotorVehicles,aes(year,total))
g + geom_point(size=4, alpha=1/2) + 
  geom_smooth(method="lm") +
  labs(title= "PM(2.5) Emissions in Baltimore, MD  due to Motor Vehicles") + 
  labs(x="Year", y="Total Emissions(Tones)")

#8. generate png file and switch off the png device immediately
dev.copy(png, file="plot5.png", width=900, height=580)
dev.off()


