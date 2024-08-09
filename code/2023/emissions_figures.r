#########################################################################
## Generate emissions pie charts and county-level maps from 2020 NEI data 
## Create emissions trends stacked line charts from emissions trends file
#########################################################################

## Set up working environment
cat("Loading emissions data... ")
map.data <- map.data.20m$county.info[,c("fips","population","area_km")]
map.colors <- c("#FFFFCC","#C7E9B4","#7FCDBB","#41B6C4","#2C7FB8","#253494") 
trend.border <- c("chocolate","gray50","red","green4","blue")
trend.colors <- pie.colors <- c("wheat","gray90","lightpink","lightgreen","lightblue")
trend.years <- c(2002:curr.year); ny <- length(trend.years);
nei.file <- paste("NAAQS_AQ/data/",curr.year,"/NEI",nei.year,"_allpoll_bycounty_sector.csv",sep="")
trend.file <- paste("NAAQS_AQ/data/",curr.year,"/national_tier1_caps.xlsx",sep="")
nei.Rdata <- paste("NAAQS_AQ/data/",curr.year,"/NEI",nei.year,"_allpoll_bycounty_sector.Rdata",sep="")
trends.Rdata <- paste("NAAQS_AQ/data/",curr.year,"/emissions_trends_2002_",curr.year,".Rdata",sep="")
if (!dir.exists(paste("NAAQS_AQ/emissions_maps/",nei.year,sep="/"))) {
  dir.create(paste("NAAQS_AQ/emissions_maps/",nei.year,sep="/"))
}
if (!dir.exists(paste("NAAQS_AQ/emissions_piecharts",nei.year,sep="/"))) {
  dir.create(paste("NAAQS_AQ/emissions_piecharts",nei.year,sep="/"))
}
if (!dir.exists(paste("NAAQS_AQ/emissions_trends",curr.year,sep="/"))) {
  dir.create(paste("NAAQS_AQ/emissions_trends",curr.year,sep="/")) 
}

if (!file.exists(nei.Rdata)) {
  ## Generate sector and county-level total dataset from NEI data for each pollutant
  poll.list <- c("Ammonia","Carbon Monoxide","Lead","Nitrogen Oxides","PM10 Primary (Filt + Cond)",
    "PM2.5 Primary (Filt + Cond)","Sulfur Dioxide","Volatile Organic Compounds")
  t <- subset(read.csv(nei.file),pollutant.desc %in% poll.list,c("fips.code","state","county",
    "pollutant.desc","sector","total.emissions","emissions.uom"))
  t$fips <- sapply(t$fips.code,function(x) ifelse(nchar(x) == 5,x,paste("0",x,sep="")))
  t$poll <- sapply(t$pollutant.desc,function(x) switch(substr(x,1,3),Amm="NH3",
    Car="CO",Lea="Pb",Nit="NOx",PM1="PM10",PM2="PM25",Sul="SO2",Vol="VOC"))
  vals <- ddply(t,c("poll","fips"),summarize,emissions=sum(total.emissions))
  vals <- recast(vals,fips ~ poll,id.var=c("poll","fips"),measure.var="emissions")
  county.data <- merge(map.data,vals,by="fips")
  vals <- ddply(t,c("poll","sector"),summarize,emissions=sum(total.emissions))
  sector.data <- recast(vals,sector ~ poll,id.var=c("poll","sector"),measure.var="emissions")
  save(list=c("sector.data","county.data"),file=nei.Rdata)
}

if (!file.exists(trends.Rdata)) {
  ## Retrieve emissions trends dataset from Excel spreadsheet
  trends.data <- NULL
  cols <- c(1,match(trend.years,c(NA,seq(1970,1990,5),1991:curr.year)))
  pm25.cols <- c(1,match(trend.years,c(NA,1990:curr.year)))
  trends.data$CO <- read.xlsx(trend.file,sheetName="CO",rowIndex=c(6:19,25:28,31:35),
    colIndex=cols,colClasses=c("character",rep("numeric",ny)))
  trends.data$NH3 <- read.xlsx(trend.file,sheetName="NH3",rowIndex=c(7:20,25:30,33:37),
    colIndex=pm25.cols,colClasses=c("character",rep("numeric",ny)))
  trends.data$NOx <- read.xlsx(trend.file,sheetName="NOX",rowIndex=c(6:19,25:28,31:35),
    colIndex=cols,colClasses=c("character",rep("numeric",ny)))
  trends.data$PM10 <- read.xlsx(trend.file,sheetName="PM10Primary",rowIndex=c(6:19,26:31),
    colIndex=cols,colClasses=c("character",rep("numeric",ny)))
  trends.data$PM25 <- read.xlsx(trend.file,sheetName="PM25Primary",rowIndex=c(6:19,26:31),
    colIndex=pm25.cols,colClasses=c("character",rep("numeric",ny)))
  trends.data$SO2 <- read.xlsx(trend.file,sheetName="SO2",rowIndex=c(6:19,25:28,31:35),
    colIndex=cols,colClasses=c("character",rep("numeric",ny)))
  trends.data$VOC <- read.xlsx(trend.file,sheetName="VOC",rowIndex=c(6:19,24:27,30:34),
    colIndex=cols,colClasses=c("character",rep("numeric",ny)))
  colnames(trends.data$CO) <- colnames(trends.data$NH3) <- colnames(trends.data$NOx) <- 
    colnames(trends.data$PM10) <- colnames(trends.data$PM25) <- colnames(trends.data$SO2) <- 
    colnames(trends.data$VOC) <- c("source",as.character(trend.years))
  save(trends.data,file=trends.Rdata)
}

load(nei.Rdata); load(trends.Rdata);
cat("Done.",as.character(round(Sys.time())),"\n")
cat("Generating .png images of emissions figures... ")

########################
## CH4 emissions figures
########################

## CH4 emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/CH4piechart",(curr.year-1),".png",sep="")
ch4.data <- read.csv(paste("NAAQS_AQ/data/",curr.year,"/CH4_emissions_1990_",(curr.year-1),".csv",sep=""))
vals <- ch4.data[,ncol(ch4.data)]*44092 ## Convert from millions of metric tons CO2 to U.S. tons
df <- data.frame(sector=c("Energy","Agriculture","Waste","Other"),emis=NA)
df$emis[1:3] <- vals[1:3]; df$emis[4] <- sum(vals[4:5]);
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Energy/Fossil Fuels","Agriculture","Waste Disposal/\nLandfills","Other"),
  " ",round(df$pct),"%",sep="")
pie.title <- paste("CH4 Emissions (",format(round(sum(df$emis/1000)),big.mark=",")," kTon/yr)",sep="")
png(file=file.name,width=800,height=600)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## CH4 emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/CH4emistrends2002_",(curr.year-1),".png",sep="")
ch4.data[,-1] <- ch4.data[,-1]*44.092 ## Convert from millions of metric tons CO2 to U.S. tons
colnames(ch4.data) <- c("source",1990:(curr.year-1))
ch4.trends <- rbind(ch4.data[1:3,c("source",as.character(2002:(curr.year-1)))],
                    c(NA,apply(ch4.data[4:5,as.character(2002:(curr.year-1))],2,sum)))
ch4.totals <- apply(ch4.trends[c(nrow(ch4.trends):1),-1],2,cumsum)
ch4.labels <- c("Energy/Fossil Fuels","Agriculture","Waste Disposal/Landfills",
  "Other Anthropogenic Sources")
ch4.years <- trend.years[-c(length(trend.years))]
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,(curr.year-1)),
  xlab="Inventory Year",yaxs='i',ylim=c(0,45000),ylab="CH4 Emissions (kTons/year)")
axis(side=1,at=seq(2002,(curr.year-1),3),labels=seq(2002,(curr.year-1),3))
axis(side=2,at=seq(0,45000,5000),labels=seq(0,45000,5000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,45000,45000),col="gray80")
abline(h=seq(5000,40000,5000),v=ch4.years,col="white")
polygon(x=c(ch4.years,rev(ch4.years)),y=c(ch4.totals[1,],rep(0,(ny-1))),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(ch4.totals)) {
  polygon(x=c(ch4.years,rev(ch4.years)),y=c(ch4.totals[i,],rev(ch4.totals[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(ch4.totals)) { lines(x=ch4.years,y=ch4.totals[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=ch4.labels,fill=rev(trend.colors)[-1],border=rev(trend.border)[-1],bty='n')
box(); dev.off();

#######################
## CO emissions figures
#######################

## CO county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/COemismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(20,50,100,200,500)
  density <- round(county.data$CO/county.data$area_km*2.59)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c(0,breaks+1)," - ",c((breaks),format(round(max(density)),big.mark=",")),
    " (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Carbon Monoxide Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## CO emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/COpiechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Biogenics","Wildfires","Other Fires","Fuel Comb","Industrial",
    "Onroad","Non-Road","Other"),emis=NA)
  df$emis[1] <- sector.data$CO[grep("Biogenics",sector.data$sector)]
  df$emis[2] <- sector.data$CO[grep("Wildfires",sector.data$sector)]
  df$emis[3] <- sum(sector.data$CO[c(grep("Fires - Ag",sector.data$sector),
    grep("Fires - Pre",sector.data$sector))])
  df$emis[4] <- sum(sector.data$CO[grep("Fuel Comb -",sector.data$sector)])
  df$emis[5] <- sum(sector.data$CO[grep("Industrial Processes -",sector.data$sector)])
  df$emis[6] <- sum(sector.data$CO[grep("Mobile - On-Road",sector.data$sector)])
  df$emis[7] <- sum(sector.data$CO[setdiff(grep("Mobile -",sector.data$sector),
    grep("On-Road",sector.data$sector))])
  df$emis[8] <- sum(sector.data$CO,na.rm=TRUE)-sum(df$emis[1:7])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Biogenics","Wildfires","Agricultural &\nPrescribed Fires",
    "Stationary Fuel\nCombustion","Industrial\nProcesses","Highway Vehicles",
    "Non-Road Mobile","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("CO Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## CO emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/COemistrends2002_",curr.year,".png",sep="")
source.cat <- c("HIGHWAY VEHICLES","OFF-HIGHWAY","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$CO[match(source.cat,trends.data$CO$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,95000),ylab="CO Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,95000,10000),labels=seq(0,95000,10000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,95000,95000),col="gray80")
abline(h=seq(5000,90000,5000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();

#########################
## Lead emissions figures
#########################

## Pb county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/Pbemismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(0.1,0.3,1,3,10)
  density <- round(county.data$Pb/county.data$area_km*2.59,2)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c("0.0",sprintf("%3.1f",breaks+0.01))," - ",c((sprintf("%3.1f",breaks)),
    round(max(density)))," (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Lead Emissions Density in lbs/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## Pb emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/Pbpiechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Aircraft","Industrial","Fuel Comb","Fires","Other"),emis=NA)
  df$emis[1] <- sector.data$Pb[grep("Mobile - Aircraft",sector.data$sector)]
  df$emis[2] <- sum(sector.data$Pb[grep("Industrial Processes -",sector.data$sector)])
  df$emis[3] <- sum(sector.data$Pb[grep("Fuel Comb -",sector.data$sector)],na.rm=TRUE)
  df$emis[4] <- sum(sector.data$Pb[grep("Fires -",sector.data$sector)],na.rm=TRUE)
  df$emis[5] <- sum(sector.data$Pb,na.rm=TRUE)-sum(df$emis[1:4])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Mobile - Aircraft","Industrial Processes","Stationary Fuel\nCombustion",
  "Fires","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/2000)),big.mark=",")
  pie.title <- paste("Pb Emissions (",pie.total," Tons/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## Pb emissions trends figure - data pulled from Air Trends report
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/Pbemistrends1990_",nei.year,".png",sep="")
df <- read.csv(paste("NAAQS_AQ/data/",curr.year,"/Lead_emissions_1990_",nei.year,".csv",sep=""))
if (any(is.na(df))) { df <- sapply(df,function(x) ifelse(is.na(x),0,x)) }
source.cat <- c("Highway.Vehicles","Non.Road.Mobile","Stationary.Fuel.Combustion","Industrial.and.Other.Processes")
df2 <- apply(df[,source.cat],1,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion","Industrial and Other Processes")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(1990,nei.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,5),ylab="Pb Emissions (kTons/year)")
axis(side=1,at=seq(1990,nei.year,3),labels=seq(1990,nei.year,3))
axis(side=2,at=seq(0,5,1),labels=seq(0,5,1))
polygon(x=c(1990,rep(nei.year,2),1990),y=c(0,0,5,5),col="gray80")
abline(h=seq(0.5,4.5,0.5),v=seq(1990,nei.year,3),col="white")
polygon(x=c(seq(1990,nei.year,3),seq(nei.year,1990,-3)),y=c(df2[1,],rep(0,((nei.year-1990)/3+1))),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df2)) {
  polygon(x=c(seq(1990,nei.year,3),seq(nei.year,1990,-3)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df2)) { lines(x=seq(1990,nei.year,3),y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=rev(source.labels),fill=rev(trend.colors)[-1],border=rev(trend.border)[-1],bty='n')
box(); dev.off();

########################
## NH3 emissions figures
########################

## NH3 county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/NH3emismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(2,4,7,12,20)
  density <- round(county.data$NH3/county.data$area_km*2.59,1)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c("0.0",sprintf("%3.1f",breaks+0.1))," - ",c(sprintf("%3.1f",breaks),
    round(max(density)))," (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Ammonia Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## NH3 emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/NH3piechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Livestock","Fertilizer","Wildfires","Other Fires","Waste Disposal",
    "Mobile","Fuel Comb","Other"),emis=NA)
  df$emis[1] <- sector.data$NH3[grep("Agriculture - Livestock",sector.data$sector)]
  df$emis[2] <- sector.data$NH3[grep("Agriculture - Fertilizer",sector.data$sector)]
  df$emis[3] <- sector.data$NH3[grep("Wildfires",sector.data$sector)]
  df$emis[4] <- sum(sector.data$NH3[c(grep("Fires - Ag",sector.data$sector),
    grep("Fires - Pre",sector.data$sector))])
  df$emis[5] <- sector.data$NH3[grep("Waste Disposal",sector.data$sector)]
  df$emis[6] <- sum(sector.data$NH3[grep("Mobile -",sector.data$sector)],na.rm=TRUE)
  df$emis[7] <- sum(sector.data$NH3[grep("Fuel Comb -",sector.data$sector)])
  df$emis[8] <- sum(sector.data$NH3,na.rm=TRUE)-sum(df$emis[1:7])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Livestock Waste","Fertilizer Application","Wildfires",
    "Agricultural & Prescribed\nFires","Waste Disposal","Mobile Sources",
    "Stationary Fuel\nCombustion","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("NH3 Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## NH3 emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/NH3emistrends2002_",curr.year,".png",sep="")
source.cat <- c("Transportation","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$NH3[match(source.cat,trends.data$NH3$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Mobile Sources","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,6000),ylab="NH3 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,6000,1000),labels=seq(0,6000,1000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,6000,6000),col="gray80")
abline(h=seq(500,5500,500),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topleft",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

########################
## NOx emissions figures
########################

## NOx county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/NOXemismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(2,5,10,20,50)
  density <- round(county.data$NOx/county.data$area_km*2.59,1)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c("0.0",sprintf("%3.1f",breaks+0.1))," - ",c(sprintf("%3.1f",breaks),
    round(max(density)))," (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Nitrogen Oxides Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## NOx emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/NOXpiechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Biogenics","Fires","Fuel Comb","Industrial",
    "Onroad","Non-Road","Other"),emis=NA)
  df$emis[1] <- sector.data$NOx[grep("Biogenics -",sector.data$sector)]
  df$emis[2] <- sum(sector.data$NOx[grep("Fires -",sector.data$sector)])
  df$emis[3] <- sum(sector.data$NOx[grep("Fuel Comb",sector.data$sector)])
  df$emis[4] <- sum(sector.data$NOx[grep("Industrial Processes -",sector.data$sector)])
  df$emis[5] <- sum(sector.data$NOx[grep("Mobile - On-Road",sector.data$sector)])
  df$emis[6] <- sum(sector.data$NOx[setdiff(grep("Mobile -",sector.data$sector),
    grep("On-Road",sector.data$sector))])
  df$emis[7] <- sum(sector.data$NOx,na.rm=TRUE)-sum(df$emis[1:6])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Biogenics","All Fires","Stationary Fuel Combustion","Industrial\nProcesses",
    "Highway Vehicles","Non-Road Mobile","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("NOx Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## NOx emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/NOXemistrends2002_",curr.year,".png",sep="")
source.cat <- c("HIGHWAY VEHICLES","OFF-HIGHWAY","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$NOx[match(source.cat,trends.data$NOx$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,26000),ylab="NOx Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,26000,4000),labels=seq(0,26000,4000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,26000,26000),col="gray80")
abline(h=seq(2000,24000,2000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();

#########################
## PM10 emissions figures
#########################

## PM10 county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/PM10emismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(5,10,20,50,100)
  density <- round(county.data$PM10/county.data$area_km*2.59,1)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c("0.0",sprintf("%3.1f",breaks+0.1))," - ",c(sprintf("%3.1f",breaks),
    round(max(density)))," (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Primary PM10 Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## PM10 emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/PM10piechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Wildfires","Other Fires","Unpaved Roads","Paved Roads","Construction",
    "Agriculture","Industrial","Fuel Comb","Mobile","Other"),emis=NA)
  df$emis[1] <- sector.data$PM10[grep("Wildfires",sector.data$sector)]
  df$emis[2] <- sum(sector.data$PM10[c(grep("Fires - Ag",sector.data$sector),
    grep("Fires - Pre",sector.data$sector))])
  df$emis[3] <- sector.data$PM10[grep("Unpaved Road Dust",sector.data$sector)]
  df$emis[4] <- sector.data$PM10[grep("Paved Road Dust",sector.data$sector)]
  df$emis[5] <- sector.data$PM10[grep("Construction Dust",sector.data$sector)]
  df$emis[6] <- sector.data$PM10[grep("Agriculture - Crops",sector.data$sector)]
  df$emis[7] <- sum(sector.data$PM10[grep("Industrial Processes -",sector.data$sector)])
  df$emis[8] <- sum(sector.data$PM10[grep("Fuel Comb -",sector.data$sector)])
  df$emis[9] <- sum(sector.data$PM10[grep("Mobile -",sector.data$sector)])
  df$emis[10] <- sum(sector.data$PM10,na.rm=TRUE)-sum(df$emis[1:9])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Wildfires","Agricultural & Prescribed\nFires","Unpaved Road\nDust",
    "Paved Road\nDust","Construction Dust","Crops & Livestock Dust","Industrial\nProcesses",
    "Stationary Fuel\nCombustion","Mobile Sources","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("Primary PM10 Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## PM10 emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/PM10emistrends2002_",curr.year,".png",sep="")
t <- trends.data$PM10
t <- rbind(t,c(NA,apply(t[1:3,-1],2,sum)),c(NA,apply(t[4:10,-1],2,sum)),
  c(NA,apply(t[11:12,-1],2,sum)))
t$source[20:22] <- c("Stationary fuel combustion","Industrial and other processes","Transportation")
source.cat <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation","Miscellaneous without wildfires")
df <- t[match(source.cat,t$source),c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Stationary Fuel Combustion","Industrial and Other Processes",
  "Transportation","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,20000),ylab="Primary PM10 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,20000,2000),labels=seq(0,20000,2000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,20000,20000),col="gray80")
abline(h=seq(1000,19000,1000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

##########################
## PM2.5 emissions figures
##########################

## PM2.5 county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/PM25emismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(1,2,5,10,20)
  density <- round(county.data$PM25/county.data$area_km*2.59,1)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c("0.0",sprintf("%3.1f",breaks+0.1))," - ",c(sprintf("%3.1f",breaks),
    round(max(density)))," (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Primary PM2.5 Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## PM2.5 emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/PM25piechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Wildfires","Other Fires","Unpaved Roads","Paved Roads","Construction",
    "Agriculture","Fuel Comb","Industrial","Mobile","Waste Disposal","Other"),emis=NA)
  df$emis[1] <- sector.data$PM25[grep("Wildfires",sector.data$sector)]
  df$emis[2] <- sum(sector.data$PM25[c(grep("Fires - Ag",sector.data$sector),
    grep("Fires - Pre",sector.data$sector))])
  df$emis[3] <- sector.data$PM25[grep("Unpaved Road Dust",sector.data$sector)]
  df$emis[4] <- sector.data$PM25[grep("Paved Road Dust",sector.data$sector)]
  df$emis[5] <- sector.data$PM25[grep("Construction Dust",sector.data$sector)]
  df$emis[6] <- sector.data$PM25[grep("Agriculture - Crops",sector.data$sector)]
  df$emis[7] <- sum(sector.data$PM25[grep("Fuel Comb -",sector.data$sector)])
  df$emis[8] <- sum(sector.data$PM25[grep("Industrial Processes -",sector.data$sector)])
  df$emis[9] <- sum(sector.data$PM25[grep("Mobile -",sector.data$sector)])
  df$emis[10] <- sector.data$PM25[grep("Waste Disposal",sector.data$sector)]
  df$emis[11] <- sum(sector.data$PM25,na.rm=TRUE)-sum(df$emis[1:10])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Wildfires","Agricultural &\nPrescribed\nFires","Unpaved Road\nDust",
    "Paved Road\nDust","Construction Dust","Crops & Livestock\nDust","Stationary Fuel Combustion",
    "Industrial Processes","Mobile Sources","Waste Disposal","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("Primary PM2.5 Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors[c(rep(1:5,2),3)],radius=0.9)
  dev.off()
}

## PM2.5 emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/PM25emistrends2002_",curr.year,".png",sep="")
t <- trends.data$PM25
t <- rbind(t,c(NA,apply(t[1:3,-1],2,sum)),c(NA,apply(t[4:10,-1],2,sum)),
  c(NA,apply(t[11:12,-1],2,sum)))
t$source[20:22] <- c("Stationary fuel combustion","Industrial and other processes","Transportation")
source.cat <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation","Miscellaneous without wildfires")
df <- t[match(source.cat,t$source),c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Stationary Fuel Combustion","Industrial and Other Processes",
  "Transportation","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,6000),ylab="PM2.5 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,6000,1000),labels=seq(0,6000,1000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,6000,6000),col="gray80")
abline(h=seq(500,5500,500),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

########################
## SO2 emissions figures
########################

## SO2 county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/SO2emismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(0.3,1,3,10,30)
  density <- round(county.data$SO2/county.data$area_km*2.59,2)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c("0.0",sprintf("%3.1f",breaks+0.1))," - ",c(sprintf("%3.1f",breaks),
    round(max(density)))," (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Sulfur Dioxide Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## SO2 emissions pie chart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/SO2piechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Wildfires","Other Fires","Fuel Comb Coal","Fuel Comb Other",
    "Industrial","Mobile","Other"),emis=NA)
  df$emis[1] <- sector.data$SO2[grep("Wildfires",sector.data$sector)]
  df$emis[2] <- sum(sector.data$SO2[c(grep("Fires - Ag",sector.data$sector),
    grep("Fires - Pre",sector.data$sector))])
  df$emis[3] <- sum(sector.data$SO2[intersect(grep("Fuel Comb",sector.data$sector),
    grep("Coal",sector.data$sector))])
  df$emis[4] <- sum(sector.data$SO2[setdiff(grep("Fuel Comb",sector.data$sector),
    grep("Coal",sector.data$sector))])
  df$emis[5] <- sum(sector.data$SO2[grep("Industrial Processes -",sector.data$sector)])
  df$emis[6] <- sum(sector.data$SO2[grep("Mobile -",sector.data$sector)])
  df$emis[7] <- sum(sector.data$SO2,na.rm=TRUE)-sum(df$emis[1:6])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Wildfires","Agricultural &\nPrescribed Fires",
    "Stationary Fuel\nCombustion: Coal","Stationary Fuel\nCombustion: Other",
    "Industrial\nProcesses","Mobile Sources","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("SO2 Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## SO2 emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/SO2emistrends2002_",curr.year,".png",sep="")
source.cat <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation","Miscellaneous without wildfires")
df <- trends.data$SO2[match(source.cat,trends.data$SO2$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Stationary Fuel Combustion","Industrial and Other Processes",
  "Transportation","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,16000),ylab="SO2 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,16000,2000),labels=seq(0,16000,2000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,16000,16000),col="gray80")
abline(h=seq(1000,15000,1000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

########################
## VOC emissions figures
########################

## VOC county-level emissions density map
file.name <- paste("NAAQS_AQ/emissions_maps/",nei.year,"/VOCemismap",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  breaks <- c(10,20,50,100,200)
  density <- round(county.data$VOC/county.data$area_km*2.59)
  county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
  N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
  map.legend <- paste(c(0,breaks+1)," - ",c(breaks,round(max(density))),
    " (",format(N,trim=TRUE,big.mark=","),")",sep="")
  map.title <- "Volatile Organic Compounds Emissions Density in tons/year/mi^2 (# Counties)"
  png(file=file.name,width=1500,height=900)
  layout(matrix(c(1,2),2,1),widths=1,heights=c(1,0.1))
  par(mar=c(0,0,0,0),bg="gray95")
  draw.map("state",proj.args=pa)
  for (i in 1:nrow(county.data)) {
    draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
  }
  draw.map("state",proj.args=pa,add=TRUE,lwd=2)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  legend("top",legend=map.legend,ncol=6,fill=map.colors,title=map.title,cex=2,bty='n')
  dev.off()
}

## VOC emissions piechart
file.name <- paste("NAAQS_AQ/emissions_piecharts/",nei.year,"/VOCpiechart",nei.year,".png",sep="")
if (!file.exists(file.name)) {
  df <- data.frame(sector=c("Biogenics","Wildfires","Other Fires","Solvents","Industrial","Mobile","Other"),emis=NA)
  df$emis[1] <- sector.data$VOC[grep("Biogenics",sector.data$sector)]
  df$emis[2] <- sector.data$VOC[grep("Wildfires",sector.data$sector)]
  df$emis[3] <- sum(sector.data$VOC[c(grep("Fires - Ag",sector.data$sector),grep("Fires - Pre",sector.data$sector))])
  df$emis[4] <- sum(sector.data$VOC[grep("Solvent -",sector.data$sector)])
  df$emis[5] <- sum(sector.data$VOC[grep("Industrial Processes",sector.data$sector)])
  df$emis[6] <- sum(sector.data$VOC[grep("Mobile - ",sector.data$sector)])
  df$emis[7] <- sum(sector.data$VOC,na.rm=TRUE)-sum(df$emis[1:6])
  df$pct <- 100*df$emis/sum(df$emis)
  pie.labels <- paste(c("Biogenics","Wildfires","Agricultural & Prescribed Fires","Solvent Utilization",
    "Industrial\nProcesses","Mobile\nSources","Other")," ",round(df$pct),"%",sep="")
  pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
  pie.title <- paste("VOC Emissions (",pie.total," kTon/year)",sep="")
  png(file=file.name,width=800,height=600)
  par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
  pie.chart(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
  dev.off()
}

## VOC emissions trends figure
file.name <- paste("NAAQS_AQ/emissions_trends/",curr.year,"/VOCemistrends2002_",curr.year,".png",sep="")
source.cat <- c("HIGHWAY VEHICLES","OFF-HIGHWAY","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$VOC[match(source.cat,trends.data$VOC$source),c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
png(file=file.name,width=1200,height=800)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2,bg="gray95")
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,18000),ylab="VOC Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,18000,2000),labels=seq(0,18000,2000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,18000,18000),col="gray80")
abline(h=seq(1000,17000,1000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();

cat("Done.",as.character(round(Sys.time())),"\n")