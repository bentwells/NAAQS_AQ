#############################################################
## ostrta.r - One script to rule them all!
## Extracts AQS data, generates PNG figures, and renders PDFs
## Author: Ben Wells, U.S. EPA/OAR/OAQPS
## Last Updated: June 14, 2024
#############################################################

## Set up working environment
cat("Setting up working environment...")
options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(fields);  library(plyr); 
library(rmarkdown); library(reshape2); library(trend); library(xlsx);
if (dir.exists("C:/")) {
  find_pandoc(dir="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
  source("Census/shape2020/rmapfuns.r"); source("R/draw_boxplots.r"); source("R/piechart.r");
}
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
pa <- list(projection='albers',parameters=c(33,45),orientation=c(90,0,-100))
curr.year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
nei.year <- 3*((curr.year-3) %/% 3)+1
if (!dir.exists(paste("NAAQS_AQ/markdown",curr.year,sep="/"))) { 
  dir.create(paste("NAAQS_AQ/markdown",curr.year,sep="/"))
  system(paste("cp NAAQS_AQ/markdown/",(curr.year-1),"/CO_",(curr.year-1),".Rmd ",
    "NAAQS_AQ/markdown/",curr.year,"/CO_",curr.year,".Rmd",sep=""))
  system(paste("cp NAAQS_AQ/markdown/",(curr.year-1),"/NO2_",(curr.year-1),".Rmd ",
    "NAAQS_AQ/markdown/",curr.year,"/NO2_",curr.year,".Rmd",sep=""))
  system(paste("cp NAAQS_AQ/markdown/",(curr.year-1),"/O3_",(curr.year-1),".Rmd ",
    "NAAQS_AQ/markdown/",curr.year,"/O3_",curr.year,".Rmd",sep=""))
  system(paste("cp NAAQS_AQ/markdown/",(curr.year-1),"/Pb_",(curr.year-1),".Rmd ",
    "NAAQS_AQ/markdown/",curr.year,"/Pb_",curr.year,".Rmd",sep=""))
  system(paste("cp NAAQS_AQ/markdown/",(curr.year-1),"/PM_",(curr.year-1),".Rmd ",
    "NAAQS_AQ/markdown/",curr.year,"/PM_",curr.year,".Rmd",sep=""))
  system(paste("cp NAAQS_AQ/markdown/",(curr.year-1),"/SO2_",(curr.year-1),".Rmd ",
    "NAAQS_AQ/markdown/",curr.year,"/SO2_",curr.year,".Rmd",sep=""))
}
cat("Done.",as.character(round(Sys.time())),"\n")

## Step 1: Download most recent emissions data files (manual), generate PNG images of emissions graphics
## a) Most recent NEI: download from https://www.epa.gov/air-emissions-inventories
##    2020 file https://gaftp.epa.gov/air/nei/2020/data_summaries/2020neiMar_county_tribe_allsector.zip
## b) Emissions trends: download from https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data
##    2022 file: https://www.epa.gov/system/files/other-files/2023-04/national_tier1_caps_05Apr2023.xlsx
## c) Lead trends: download from https://www.epa.gov/air-trends, click on latest trends report link
##    2020 file: https://gispub.epa.gov/air/trendsreport/2022/#naaqs_trends
## d) Methane: download from https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks
##    2021 file: https://cfpub.epa.gov/ghgdata/inventoryexplorer/#
source(paste("NAAQS_AQ/code/",curr.year,"/emissions_figures.r",sep=""))

## Step 2: Retrieve AQ monitor and concentration data from AQS, generate AQ concentration tables
## Notes: 1) Must be run on a system set up to run SQL queries to AQS in R
##        2) These scripts usually take several hours to run
source(paste("NAAQS_AQ/code/",curr.year,"/aqs_data_retrieval.r",sep=""))
source(paste("NAAQS_AQ/code/",curr.year,"/conc_tables.r",sep=""))

## Step 3: Generate PNG images of AQ graphics
source(paste("NAAQS_AQ/code/",curr.year,"/monitor_maps.r",sep=""))
source(paste("NAAQS_AQ/code/",curr.year,"/dv_maps.r",sep=""))
source(paste("NAAQS_AQ/code/",curr.year,"/trends_figures.r",sep=""))
source(paste("NAAQS_AQ/code/",curr.year,"/speciation_piecharts.r",sep=""))

## Step 4: Render AQ documents in HTML and PDF format using rmarkdown
cat("Rendering PDF versions of AQ documents...")
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/CO_",curr.year,".Rmd",sep=""),
  output_format="pdf_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
detach("package:kableExtra",unload=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/NO2_",curr.year,".Rmd",sep=""),
  output_format="pdf_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
detach("package:kableExtra",unload=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/O3_",curr.year,".Rmd",sep=""),
  output_format="pdf_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
detach("package:kableExtra",unload=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/Pb_",curr.year,".Rmd",sep=""),
  output_format="pdf_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
detach("package:kableExtra",unload=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/PM_",curr.year,".Rmd",sep=""),
  output_format="pdf_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
detach("package:kableExtra",unload=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/SO2_",curr.year,".Rmd",sep=""),
  output_format="pdf_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
cat("Done.",as.character(round(Sys.time())),"\n")

cat("Rendering HTML versions of AQ documents...")
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/CO_",curr.year,".Rmd",sep=""),
  output_format="html_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/NO2_",curr.year,".Rmd",sep=""),
  output_format="html_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/O3_",curr.year,".Rmd",sep=""),
  output_format="html_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/Pb_",curr.year,".Rmd",sep=""),
  output_format="html_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/PM_",curr.year,".Rmd",sep=""),
  output_format="html_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
render(input=paste("NAAQS_AQ/markdown/",curr.year,"/SO2_",curr.year,".Rmd",sep=""),
  output_format="html_document",params=list(curr.year=curr.year,nei.year=nei.year),quiet=TRUE)
cat("Done.",as.character(round(Sys.time())),"\n")