##########################################################
## Generate AQ concentration tables for NAAQS AQ documents
##########################################################

## Set up working environment
cat("Preparing to generate AQ concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))

## Functions used to generate table summaries
get.quarter <- function(d) { 
  q <- (as.integer(substr(d,6,7)) - 1) %/% 3 + 1
  qc <- paste(q,ifelse(q == 1,"st",ifelse(q == 2,"nd",ifelse(q == 3,"rd","th"))),sep="")
  return(paste(qc,"quarter"))
}
get.region <- function(id) {
  return(switch(paste("s",substr(id,1,2),sep=""),
    s01="Southeast",s02="Northwest",s04="Southwest",s05="South",s06="West",s08="Southwest",
    s09="Northeast",s10="Northeast",s11="Northeast",s12="Southeast",s13="Southeast",s15="West",
    s16="Northwest",s17="Central",s18="Central",s19="East North Central",s20="South",s21="Central",
    s22="South",s23="Northeast",s24="Northeast",s25="Northeast",s26="East North Central",
    s27="East North Central",s28="South",s29="Central",s30="West North Central",
    s31="West North Central",s32="West",s33="Northeast",s34="Northeast",s35="Southwest",
    s36="Northeast",s37="Southeast",s38="West North Central",s39="Central",s40="South",
    s41="Northwest",s42="Northeast",s44="Northeast",s45="Southeast",s46="West North Central",
    s47="Southeast",s48="South",s49="Southwest",s50="Northeast",s51="Southeast",s53="Northwest",
    s54="Central",s55="East North Central",s56="West North Central",s72="Southeast",NA))
}
get.season <- function(d) {
  return(switch(paste("m",substr(d,6,7),sep=""),
    m01="winter",m02="winter",m03="spring",m04="spring",m05="spring",m06="summer",
    m07="summer",m08="summer",m09="autumn",m10="autumn",m11="autumn",m12="winter"))
}
get.stats <- function(df,metric,region,season,format="%f") {
  df$val <- df[,metric]
  if (region != "all") { df <- df[which(df$region == region),] }
  if (season != "all") { df <- df[which(df$season == season),] }
  pct <- ddply(df,"site",summarize,pct=round(100*sum(!is.na(val))/length(val)))
  sites.keep <- pct$site[which(pct$pct >= 75)]
  x <- df$val[which(df$site %in% sites.keep)]
  q <- sprintf(format,quantile(x,probs=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.98,0.99,1),na.rm=TRUE))
  names(q) <- c("min",paste("p",c(1,5,10,25,50,75,90,95,98,99),sep=""),"max")
  return(data.frame(metric,region,season,N.sites=length(sites.keep),
    N.obs=sum(!is.na(x)),mean=sprintf(format,mean(x,na.rm=TRUE)),SD=sprintf(format,sd(x,na.rm=TRUE)),
    t(q),max.site=df$site[which.max(df$val)]))
}
cat("Done.",as.character(round(Sys.time())),"\n")

########################################################################
## CO: Calculate summary statistics based on MDA1, MDA8 and DA24 metrics
########################################################################
cat("Generating CO concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/COdaily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(co.daily,c("site","date"),summarize,MDA1=max.na(conc.mda1),
  MDA8=max.na(conc.mda8),DA24=max.na(conc))
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA8))/length(MDA8)))
daily <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])

## Table 1: Daily 1-hr max, 8-hr max, 24-hr mean aggregated by season
co.table1 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%5.2f"),
  get.stats(df=daily,metric="MDA1",region="all",season="winter",format="%5.2f"),
  get.stats(df=daily,metric="MDA1",region="all",season="spring",format="%5.2f"),
  get.stats(df=daily,metric="MDA1",region="all",season="summer",format="%5.2f"),
  get.stats(df=daily,metric="MDA1",region="all",season="autumn",format="%5.2f"),
  get.stats(df=daily,metric="MDA8",region="all",season="all",format="%5.2f"),
  get.stats(df=daily,metric="MDA8",region="all",season="winter",format="%5.2f"),
  get.stats(df=daily,metric="MDA8",region="all",season="spring",format="%5.2f"),
  get.stats(df=daily,metric="MDA8",region="all",season="summer",format="%5.2f"),
  get.stats(df=daily,metric="MDA8",region="all",season="autumn",format="%5.2f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%5.2f"),
  get.stats(df=daily,metric="DA24",region="all",season="winter",format="%5.2f"),
  get.stats(df=daily,metric="DA24",region="all",season="spring",format="%5.2f"),
  get.stats(df=daily,metric="DA24",region="all",season="summer",format="%5.2f"),
  get.stats(df=daily,metric="DA24",region="all",season="autumn",format="%5.2f"))

## Table 2: Daily 1-hr max, 8-hr max, 24-hr mean aggregated by site type
co.monitors$site <- substr(co.monitors$id,1,9)
sites <- subset(co.monitors,!duplicated(site))
nroad <- subset(daily,site %in% sites$site[which(sites$network == "NEAR ROAD")])
urban <- subset(daily,site %in% sites$site[which(sites$network == "NCORE" & sites$measurement_scale != "Regional Scale")])
rural <- subset(daily,site %in% sites$site[which(sites$measurement_scale == "Regional Scale")])
co.table2 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%5.2f"),
  get.stats(df=nroad,metric="MDA1",region="all",season="all",format="%5.2f"),
  get.stats(df=urban,metric="MDA1",region="all",season="all",format="%5.2f"),
  get.stats(df=rural,metric="MDA1",region="all",season="all",format="%5.2f"),
  get.stats(df=daily,metric="MDA8",region="all",season="all",format="%5.2f"),
  get.stats(df=nroad,metric="MDA8",region="all",season="all",format="%5.2f"),
  get.stats(df=urban,metric="MDA8",region="all",season="all",format="%5.2f"),
  get.stats(df=rural,metric="MDA8",region="all",season="all",format="%5.2f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%5.2f"),
  get.stats(df=nroad,metric="DA24",region="all",season="all",format="%5.2f"),
  get.stats(df=urban,metric="DA24",region="all",season="all",format="%5.2f"),
  get.stats(df=rural,metric="DA24",region="all",season="all",format="%5.2f"))
colnames(co.table2)[2] <- "site.type"; 
co.table2$site.type <- rep(c("All Sites","Near Road","Urban NCore","Rural"),times=3)
cat("Done.",as.character(round(Sys.time())),"\n")

########################################################################################################
## Lead: Calculate summary statistics based on daily samples, monthly averages, 3-month rolling averages
########################################################################################################
cat("Generating Pb concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/PBdaily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(pb.daily,c("parameter","site","date"),summarize,conc=max.na(conc))
temp$season <- sapply(temp$date,get.quarter)
temp$month <- substr(temp$date,1,7)
daily <- subset(temp,parameter == "14129" & !is.na(conc))
monthly <- ddply(daily,c("site","month"),summarize,conc=mean(conc),conc.3mo=NA,season=season[1])
for (i in 3:nrow(monthly)) {
  if (monthly$site[i] != monthly$site[i-2]) { next }
  monthly$conc.3mo[i] <- mean(monthly$conc[(i-2):i])
}
daily <- subset(daily,as.numeric(substr(date,1,4)) > curr.year-3)
monthly <- subset(monthly,as.numeric(substr(month,1,4)) > curr.year-3)

## Table 1: Daily, monthly, 3-month values aggregated by quarter
pb.table1 <- rbind(
  get.stats(df=daily,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=daily,metric="conc",region="all",season="1st quarter",format="%5.3f"),
  get.stats(df=daily,metric="conc",region="all",season="2nd quarter",format="%5.3f"),
  get.stats(df=daily,metric="conc",region="all",season="3rd quarter",format="%5.3f"),
  get.stats(df=daily,metric="conc",region="all",season="4th quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly,metric="conc",region="all",season="1st quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc",region="all",season="2nd quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc",region="all",season="3rd quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc",region="all",season="4th quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="1st quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="2nd quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="3rd quarter",format="%5.3f"),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="4th quarter",format="%5.3f"))
pb.table1$metric <- rep(c("daily","monthly","3-month"),each=5)
colnames(pb.table1)[3] <- "quarter"

pb.monitors$parameter <- "14129"
mc <- intersect(colnames(pb.monitors),colnames(pb.nonreg.monitors))
monitors <- rbind(pb.monitors[,mc],pb.nonreg.monitors[,mc])
monitors$site <- substr(monitors$id,1,9)
sites <- subset(monitors,!duplicated(paste(parameter,site)))
daily <- subset(temp,!is.na(conc))
monthly <- ddply(daily,c("parameter","site","month"),summarize,conc=mean(conc),conc.3mo=NA,season=season[1])
for (i in 3:nrow(monthly)) {
  if (monthly$site[i] != monthly$site[i-2]) { next }
  monthly$conc.3mo[i] <- mean(monthly$conc[(i-2):i])
}
daily <- subset(daily,as.numeric(substr(date,1,4)) > curr.year-3)
monthly <- subset(monthly,as.numeric(substr(month,1,4)) > curr.year-3)
tsp.so <- subset(sites,parameter %in% c("12128","14129") & monitor_objective %in%
  c("Highest Concentration","Source Oriented"))
tsp.nso <- subset(sites,parameter %in% c("12128","14129") & !(monitor_objective %in%
  c("Highest Concentration","Source Oriented")))
pm10 <- subset(sites,parameter %in% c("82128","85128"))
pm25.rural <- subset(sites,parameter == "88128" & network == "IMPROVE")
pm25.urban <- subset(sites,parameter == "88128" & network != "IMPROVE")
daily.tsp.so <- subset(daily,parameter %in% c("12128","14129") & site %in% tsp.so$site)
daily.tsp.nso <- subset(daily,parameter %in% c("12128","14129") & site %in% tsp.nso$site)
daily.pm10 <- subset(daily,parameter %in% c("82128","85128") & site %in% pm10$site)
daily.pm25.urban <- subset(daily,parameter == "88128" & site %in% pm25.urban$site)
daily.pm25.rural <- subset(daily,parameter == "88128" & site %in% pm25.rural$site)
monthly.tsp.so <- subset(monthly,parameter %in% c("12128","14129") & site %in% tsp.so$site)
monthly.tsp.nso <- subset(monthly,parameter %in% c("12128","14129") & site %in% tsp.nso$site)
monthly.pm10 <- subset(monthly,parameter %in% c("82128","85128") & site %in% pm10$site)
monthly.pm25.urban <- subset(monthly,parameter == "88128" & site %in% pm25.urban$site)
monthly.pm25.rural <- subset(monthly,parameter == "88128" & site %in% pm25.rural$site)

## Table 2: Daily, monthly, 3-month values aggregated by monitoring network
pb.table2 <- rbind(
  get.stats(df=daily.tsp.so,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=daily.tsp.nso,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=daily.pm10,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=daily.pm25.urban,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=daily.pm25.rural,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.tsp.so,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.tsp.nso,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.pm10,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.pm25.urban,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.pm25.rural,metric="conc",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.tsp.so,metric="conc.3mo",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.tsp.nso,metric="conc.3mo",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.pm10,metric="conc.3mo",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.pm25.urban,metric="conc.3mo",region="all",season="all",format="%5.3f"),
  get.stats(df=monthly.pm25.rural,metric="conc.3mo",region="all",season="all",format="%5.3f"))
pb.table2$metric <- rep(c("daily","monthly","3-month"),each=5)
pb.table2$region <- rep(c("Pb-TSP","Pb-TSP","Pb-PM10","Pb-PM2.5","Pb-PM2.5"),times=3)
pb.table2$season <- rep(c("Source","Non-Source","All Sites","Urban","Rural"),times=3)
colnames(pb.table2)[2:3] <- c("measurement","network")
cat("Done.",as.character(round(Sys.time())),"\n")

###################################################################
## NO2: Calculate summary statistics based on MDA1 and DA24 metrics
###################################################################
cat("Generating NO2 concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/NO2daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(no2.daily,c("site","date"),summarize,MDA1=max.na(conc.max),DA24=max.na(conc.mean))
temp$region <- sapply(temp$site,get.region)
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA1))/length(MDA1)))
daily <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])

## Table 1: Daily 1-hr max and 24-hr mean aggregated by season 
no2.table1 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="winter",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="spring",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="summer",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="autumn",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="winter",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="spring",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="summer",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="autumn",format="%5.1f"))

## Table 2: Daily 1-hr max and 24-hr mean aggregated by NOAA climate region
no2.table2 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="Central",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="East North Central",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="Northeast",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="Northwest",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="South",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="Southeast",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="Southwest",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="West",season="all",format="%5.1f"),
  get.stats(df=daily,metric="MDA1",region="West North Central",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="Central",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="East North Central",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="Northeast",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="Northwest",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="South",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="Southeast",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="Southwest",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="West",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="West North Central",season="all",format="%5.1f"))

## Table 3: Daily 1-hr max and 24-hr mean aggregated by site type
no2.monitors$site <- substr(no2.monitors$id,1,9)
sites <- subset(no2.monitors,!duplicated(site))
nroad <- subset(daily,site %in% sites$site[which(sites$network == "NEAR ROAD")])
urban <- subset(daily,site %in% sites$site[which((grepl("NCORE",sites$network) | grepl("PAMS",sites$network)) &
   sites$measurement_scale != "Regional Scale")])
rural <- subset(daily,site %in% sites$site[which(sites$measurement_scale == "Regional Scale")])
no2.table3 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%5.1f"),
  get.stats(df=nroad,metric="MDA1",region="all",season="all",format="%5.1f"),
  get.stats(df=urban,metric="MDA1",region="all",season="all",format="%5.1f"),
  get.stats(df=rural,metric="MDA1",region="all",season="all",format="%5.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%5.1f"),
  get.stats(df=nroad,metric="DA24",region="all",season="all",format="%5.1f"),
  get.stats(df=urban,metric="DA24",region="all",season="all",format="%5.1f"),
  get.stats(df=rural,metric="DA24",region="all",season="all",format="%5.1f"))
colnames(no2.table3)[2] <- "site.type"
no2.table3$site.type <- rep(c("All Sites","Near Road","NCore/PAMS","Rural Sites"),times=2)
cat("Done.",as.character(round(Sys.time())),"\n")

#######################################################################################
## Ozone: Calculate summary statistics based on 'year-round' and 'warm season' datasets
#######################################################################################
cat("Generating O3 concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/O3daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- subset(o3.daily,as.numeric(substr(site,1,2)) <= 56)
temp <- ddply(temp,c("site","date"),summarize,MDA1=max.na(conc.mda1),
  MDA8=max.na(conc.mda8),DA24=max.na(conc.mean))
temp$region <- sapply(temp$site,get.region)
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA8))/length(MDA8)))
daily.yr <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])

## Table 1: Year-round dataset aggregated by season
o3.table1 <- rbind(
  get.stats(df=daily.yr,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="winter",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="spring",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="summer",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="autumn",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="winter",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="spring",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="summer",format="%3.0f"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="autumn",format="%3.0f"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="winter",format="%3.0f"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="spring",format="%3.0f"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="summer",format="%3.0f"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="autumn",format="%3.0f"))

## Table 2: Warm season dataset aggregated by NOAA climate region
temp <- subset(temp,as.numeric(substr(date,6,7)) %in% c(5:9))
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA8))/length(MDA8)))
daily.ws <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])
o3.table2 <- rbind(
  get.stats(df=daily.ws,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="East North Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="Northeast",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="Northwest",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="South",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="Southeast",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="Southwest",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="West",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA1",region="West North Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="East North Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="Northeast",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="Northwest",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="South",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="Southeast",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="Southwest",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="West",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="West North Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="East North Central",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="Northeast",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="Northwest",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="South",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="Southeast",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="Southwest",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="West",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="West North Central",season="all",format="%3.0f"))

## Table 3: Warm season dataset aggregated by site type
o3.monitors$site <- substr(o3.monitors$id,1,9)
sites <- subset(o3.monitors,!duplicated(site))
rural.east <- subset(daily.ws,site %in% sites$site[which(sites$network == "CASTNET" & as.numeric(sites$longitude) > -100)])
urban.east <- subset(daily.ws,site %in% sites$site[which((grepl("NCORE",sites$network) | grepl("PAMS",sites$network)) &
   sites$measurement_scale != "Regional Scale" & as.numeric(sites$longitude) > -100)])
rural.west <- subset(daily.ws,site %in% sites$site[which(sites$network == "CASTNET" & as.numeric(sites$longitude) < -100)])
urban.west <- subset(daily.ws,site %in% sites$site[which((grepl("NCORE",sites$network) | grepl("PAMS",sites$network)) &
   sites$measurement_scale != "Regional Scale" & as.numeric(sites$longitude) < -100)])
o3.table3 <- rbind(
  get.stats(df=daily.ws,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=urban.east,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=rural.east,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=urban.west,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=rural.west,metric="MDA1",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=urban.east,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=rural.east,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=urban.west,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=rural.west,metric="MDA8",region="all",season="all",format="%3.0f"),
  get.stats(df=daily.ws,metric="DA24",region="all",season="all",format="%3.0f"),
  get.stats(df=urban.east,metric="DA24",region="all",season="all",format="%3.0f"),
  get.stats(df=rural.east,metric="DA24",region="all",season="all",format="%3.0f"),
  get.stats(df=urban.west,metric="DA24",region="all",season="all",format="%3.0f"),
  get.stats(df=rural.west,metric="DA24",region="all",season="all",format="%3.0f"))
colnames(o3.table3)[3] <- "site.type"
o3.table3$region <- rep(c("All Sites",rep("Eastern U.S.",2),rep("Western U.S.",2)),times=3)
o3.table3$site.type <- rep(c("All Sites",rep(c("Urban","Rural"),2)),times=3)
cat("Done.",as.character(round(Sys.time())),"\n")

###############################################################################
## PM: Calculate summary statistics for PM10, PM2.5, PM10-2.5 and PM2.5 species
###############################################################################
cat("Generating PM concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/PM10daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/PM25daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/PM25spec_daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
pm10.mean <- na.omit(ddply(subset(pm10.daily,conc > -10),c("site","date"),summarize,DA24=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm10.mda1 <- na.omit(ddply(subset(pm10.dmax,conc > -5),c("site","date"),summarize,MDA1=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm25.mean <- na.omit(ddply(subset(pm25.daily,conc > -10),c("site","date"),summarize,DA24=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm25.mda1 <- na.omit(ddply(subset(pm25.dmax,conc > -5),c("site","date"),summarize,MDA1=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm10_25 <- na.omit(ddply(subset(pm10_25.daily,conc > -10),c("site","date"),summarize,DA24=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm25.spec <- ddply(pm25.spec.daily,c("site","date"),summarize,SO4=max.na(so4),
  NO3=max.na(no3),EC=max.na(ec),OC=max.na(oc),Crustal=max.na(crustal),
  Sea_Salt=max.na(seasalt),region=get.region(site[1]),season=get.quarter(date[1]))

## Table 1: PM10, PM2.5, and PM10-2.5 stats by quarter
pm.table1 <- rbind(
  get.stats(df=pm10.mean,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="1st quarter",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="2nd quarter",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="3rd quarter",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="4th quarter",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="1st quarter",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="2nd quarter",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="3rd quarter",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="4th quarter",format="%5.0f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="1st quarter",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="2nd quarter",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="3rd quarter",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="4th quarter",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="1st quarter",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="2nd quarter",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="3rd quarter",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="4th quarter",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="all",season="1st quarter",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="all",season="2nd quarter",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="all",season="3rd quarter",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="all",season="4th quarter",format="%6.1f"))
pm.table1$region <- rep(c("PM10","PM10","PM2.5","PM2.5","PM10-2.5"),each=5)
colnames(pm.table1)[2:3] <- c("pollutant","quarter")
pm.table1 <- pm.table1[,c(2,1,3:ncol(pm.table1))]

## Table 2: PM10, PM2.5, and PM10-2.5 stats by NOAA climate region
pm.table2 <- rbind(
  get.stats(df=pm10.mean,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="Central",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="East North Central",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="Northeast",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="Northwest",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="South",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="Southeast",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="Southwest",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="West",season="all",format="%5.0f"),
  get.stats(df=pm10.mean,metric="DA24",region="West North Central",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="Central",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="East North Central",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="Northeast",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="Northwest",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="South",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="Southeast",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="Southwest",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="West",season="all",format="%5.0f"),
  get.stats(df=pm10.mda1,metric="MDA1",region="West North Central",season="all",format="%5.0f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="Central",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="East North Central",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="Northeast",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="Northwest",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="South",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="Southeast",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="Southwest",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="West",season="all",format="%6.1f"),
  get.stats(df=pm25.mean,metric="DA24",region="West North Central",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="Central",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="East North Central",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="Northeast",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="Northwest",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="South",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="Southeast",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="Southwest",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="West",season="all",format="%6.1f"),
  get.stats(df=pm25.mda1,metric="MDA1",region="West North Central",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="Central",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="East North Central",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="Northeast",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="Northwest",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="South",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="Southeast",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="Southwest",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="West",season="all",format="%6.1f"),
  get.stats(df=pm10_25,metric="DA24",region="West North Central",season="all",format="%6.1f"))
pm.table2$season <- rep(c("PM10","PM10","PM2.5","PM2.5","PM10-2.5"),each=10)
colnames(pm.table2)[2:3] <- c("region","pollutant")
pm.table2 <- pm.table2[,c(3,1,2,4:ncol(pm.table2))]

## Table 3: PM10 and PM2.5 stats by site type
mc <- intersect(colnames(pm10.monitors),colnames(pm10.nonreg.monitors))
pm10.monitors <- rbind(pm10.monitors[,mc],pm10.nonreg.monitors[,mc])
pm10.monitors$site <- substr(pm10.monitors$id,1,9)
mc <- intersect(colnames(pm25.monitors),colnames(pm25.nonreg.monitors))
pm25.monitors <- rbind(pm25.monitors[,mc],pm25.nonreg.monitors[,mc])
pm25.monitors$site <- substr(pm25.monitors$id,1,9)
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
csn.sites <- subset(subset(pm25.spec.monitors,grepl("CSN",network)),!duplicated(site))
imp.sites <- subset(subset(pm25.spec.monitors,grepl("IMPROVE",network)),!duplicated(site))
nroad.sites <- subset(subset(pm25.monitors,grepl("NEAR ROAD",network)),!duplicated(site))
pm10.urban.east <- subset(pm10.mean,site %in% csn.sites$site[which(as.numeric(csn.sites$longitude) > -100)])
pm10.rural.east <- subset(pm10.mean,site %in% imp.sites$site[which(as.numeric(imp.sites$longitude) > -100)])
pm10.urban.west <- subset(pm10.mean,site %in% csn.sites$site[which(as.numeric(csn.sites$longitude) < -100)])
pm10.rural.west <- subset(pm10.mean,site %in% imp.sites$site[which(as.numeric(imp.sites$longitude) < -100)])
pm25.urban.east <- subset(pm25.mean,site %in% csn.sites$site[which(as.numeric(csn.sites$longitude) > -100)])
pm25.rural.east <- subset(pm25.mean,site %in% imp.sites$site[which(as.numeric(imp.sites$longitude) > -100)])
pm25.urban.west <- subset(pm25.mean,site %in% csn.sites$site[which(as.numeric(csn.sites$longitude) < -100)])
pm25.rural.west <- subset(pm25.mean,site %in% imp.sites$site[which(as.numeric(imp.sites$longitude) < -100)])
pm25.nroad <- subset(pm25.mean,site %in% nroad.sites$site)

pm.table3 <- data.frame(pollutant=c(rep("PM10",5),rep("PM2.5",6)),rbind(
  get.stats(df=pm10.mean,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.urban.east,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.rural.east,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.urban.west,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm10.rural.west,metric="DA24",region="all",season="all",format="%5.0f"),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.urban.east,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.rural.east,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.urban.west,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.rural.west,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=pm25.nroad,metric="DA24",region="all",season="all",format="%6.1f")))
colnames(pm.table3)[4] <- "network"
pm.table3$region <- c(rep(c("all",rep("Eastern U.S.",2),rep("Western U.S.",2)),2),"all")
pm.table3$network <- c(rep(c("all",rep(c("CSN","IMPROVE"),2)),2),"Near Road")

## Table 4: PM2.5 species stats by quarter
pm.table4 <- rbind(
  get.stats(df=pm25.spec,metric="SO4",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="4th quarter",format="%6.2f"))
colnames(pm.table4)[1:3] <- c("species","region","quarter")

## Table 5: PM2.5 species stats by NOAA climate region
pm.table5 <- rbind(
  get.stats(df=pm25.spec,metric="SO4",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="East North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="Northeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="Northwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="South",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="Southeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="Southwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="West",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="SO4",region="West North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="East North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="Northeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="Northwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="South",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="Southeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="Southwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="West",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="West North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="East North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="Northeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="Northwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="South",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="Southeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="Southwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="West",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="West North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="East North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="Northeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="Northwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="South",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="Southeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="Southwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="West",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="West North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="East North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="Northeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="Northwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="South",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="Southeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="Southwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="West",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="West North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="East North Central",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Northeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Northwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="South",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Southeast",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Southwest",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="West",season="all",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="West North Central",season="all",format="%6.2f"))
colnames(pm.table5)[1] <- "species"

## Table 6: PM2.5 species stats by site type
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
csn.sites <- subset(subset(pm25.spec.monitors,grepl("CSN",network)),!duplicated(site))
imp.sites <- subset(subset(pm25.spec.monitors,grepl("IMPROVE",network)),!duplicated(site))
urban.east <- subset(pm25.spec,site %in% csn.sites$site[which(as.numeric(csn.sites$longitude) > -100)])
rural.east <- subset(pm25.spec,site %in% imp.sites$site[which(as.numeric(imp.sites$longitude) > -100)])
urban.west <- subset(pm25.spec,site %in% csn.sites$site[which(as.numeric(csn.sites$longitude) < -100)])
rural.west <- subset(pm25.spec,site %in% imp.sites$site[which(as.numeric(imp.sites$longitude) < -100)])

pm.table6 <- rbind(
  get.stats(df=pm25.spec,metric="SO4",region="all",season="all",format="%6.2f"),
  get.stats(df=urban.east,metric="SO4",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=rural.east,metric="SO4",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=urban.west,metric="SO4",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=rural.west,metric="SO4",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="all",format="%6.2f"),
  get.stats(df=urban.east,metric="NO3",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=rural.east,metric="NO3",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=urban.west,metric="NO3",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=rural.west,metric="NO3",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="EC",region="all",season="all",format="%6.2f"),
  get.stats(df=urban.east,metric="EC",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=rural.east,metric="EC",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=urban.west,metric="EC",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=rural.west,metric="EC",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="OC",region="all",season="all",format="%6.2f"),
  get.stats(df=urban.east,metric="OC",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=rural.east,metric="OC",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=urban.west,metric="OC",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=rural.west,metric="OC",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="all",format="%6.2f"),
  get.stats(df=urban.east,metric="Crustal",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=rural.east,metric="Crustal",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=urban.west,metric="Crustal",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=rural.west,metric="Crustal",region="all",season="4th quarter",format="%6.2f"),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="all",format="%6.2f"),
  get.stats(df=urban.east,metric="Sea_Salt",region="all",season="1st quarter",format="%6.2f"),
  get.stats(df=rural.east,metric="Sea_Salt",region="all",season="2nd quarter",format="%6.2f"),
  get.stats(df=urban.west,metric="Sea_Salt",region="all",season="3rd quarter",format="%6.2f"),
  get.stats(df=rural.west,metric="Sea_Salt",region="all",season="4th quarter",format="%6.2f"))
colnames(pm.table6)[1:3] <- c("species","region","network")
pm.table6$region <- rep(c("all",rep("Eastern U.S.",2),rep("Western U.S.",2)),6)
pm.table6$network <- rep(c("all",rep(c("CSN","IMPROVE"),2)),6)
cat("Done.",as.character(round(Sys.time())),"\n")

###################################################################
## SO2: Calculate summary statistics based on MDA1 and DA24 metrics
###################################################################
cat("Generating SO2 concentration tables... ")
load(paste("NAAQS_AQ/data/",curr.year,"/SO2daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(so2.daily,c("site","date"),summarize,MDA1=max.na(conc.max),DA24=max.na(conc.mean))
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA1))/length(MDA1)))
daily <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])

## Table 1: Daily 1-hr max and 24-hr mean aggregated by season 
so2.table1 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="winter",format="%6.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="spring",format="%6.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="summer",format="%6.1f"),
  get.stats(df=daily,metric="MDA1",region="all",season="autumn",format="%6.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="winter",format="%6.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="spring",format="%6.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="summer",format="%6.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="autumn",format="%6.1f"))

## Table 2: Daily 1-hr max and 24-hr mean aggregated by site type
so2.monitors$site <- substr(so2.monitors$id,1,9)
sites <- subset(so2.monitors,!duplicated(site))
source <- subset(daily,site %in% sites$site[which(sites$monitor_objective %in% c("Highest Concentration","Source Oriented")
  & sites$county_name != "Hawaii")])
urban <- subset(daily,site %in% sites$site[which(!(sites$monitor_objective %in% c("Highest Concentration","Source Oriented"))
  & sites$measurement_scale != "Regional Scale" & sites$county_name != "Hawaii")])
rural <- subset(daily,site %in% sites$site[which(!(sites$monitor_objective %in% c("Highest Concentration","Source Oriented"))
  & sites$measurement_scale == "Regional Scale" & sites$county_name != "Hawaii")])
volcano <- subset(daily,site %in% sites$site[which(sites$county_name == "Hawaii")])

so2.table2 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=source,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=urban,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=rural,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=volcano,metric="MDA1",region="all",season="all",format="%6.1f"),
  get.stats(df=daily,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=source,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=urban,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=rural,metric="DA24",region="all",season="all",format="%6.1f"),
  get.stats(df=volcano,metric="DA24",region="all",season="all",format="%6.1f"))
colnames(so2.table2)[2] <- "site.type"
so2.table2$site.type <- rep(c("All","Source Oriented","Urban Non-Source","Rural Non-Source","Hawaii Volcanic"),times=2)
cat("Done.",as.character(round(Sys.time())),"\n")

## Save concentration tables to a .Rdata file for use in markdown documents
save(list=ls(pattern="table"),
  file=paste("NAAQS_AQ/data/",curr.year,"/conctables",curr.year-2,"_",curr.year,".Rdata",sep=""))
cat("AQ concentration tables complete.","\n\n")