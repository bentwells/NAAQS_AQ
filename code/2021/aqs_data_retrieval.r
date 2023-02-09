#########################################################
## Retrieve AQS Data for NAAQS AQ graphics - run on atmos
#########################################################

## Set up working environment
cat("Preparing to start AQS data retrievals...")
if (dir.exists("/home/")) { source("/home/bwells01/R/get_monitors.r") }
if (dir.exists("C:/")) { source("C:/Users/bwells01/Documents/R/get_monitors.r") }
## aqs.tables <- unlist(get.aqs.data("SELECT table_name FROM all_tables WHERE owner = 'AIRSRAQS'"))
## aqs.views <- unlist(get.aqs.data("SELECT view_name FROM all_views WHERE owner IN ('AIRSRAQS','AQSPUB')"))
pmax.na <- function(x,y) { ifelse(is.na(x) & is.na(y),NA,pmax(x,y,na.rm=TRUE)) }
merge.all <- function(x,y) { merge(x,y,all=TRUE) }
if (!dir.exists(paste("NAAQS_AQ/data",curr.year,sep="/"))) { 
  dir.create(paste("NAAQS_AQ/data",curr.year,sep="/")) 
}
cat("Done.",as.character(Sys.time()),"\n")

## Retrieve monitor metadata for most recent 3-year period for monitor maps
cat("Retrieving regulatory monitor metadata...")
co.monitors <- get.monitors(par=42101,yr1=curr.year-2,yr2=curr.year)
no2.monitors <- get.monitors(par=42602,yr1=curr.year-2,yr2=curr.year)
o3.monitors <- get.monitors(par=44201,yr1=curr.year-2,yr2=curr.year)
so2.monitors <- get.monitors(par=42401,yr1=curr.year-2,yr2=curr.year)
pm10.monitors <- get.monitors(par=81102,yr1=curr.year-2,yr2=curr.year)
pm25.monitors <- get.monitors(par=88101,yr1=curr.year-2,yr2=curr.year)
t1 <- get.monitors(par=12128,yr1=curr.year-2,yr2=curr.year); t1$parameter <- 12128;
t2 <- get.monitors(par=14129,yr1=curr.year-2,yr2=curr.year); t2$parameter <- 14129;
t3 <- get.monitors(par=85129,yr1=curr.year-2,yr2=curr.year); t3$parameter <- 85129;
pb.monitors <- rbind(t1,t2,t3)
save(list=paste(c("co","no2","o3","so2","pm10","pm25","pb"),"monitors",sep="."),
  file=paste("NAAQS_AQ/data/",curr.year,"/monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Get non-regulatory (e.g. CSN, IMPROVE) monitors operating during most recent 3-year period
cat("Retrieving non-regulatory monitor metadata...")
t1 <- get.monitors(par=82128,yr1=curr.year-2,yr2=curr.year); t1$parameter <- 82128;
t2 <- get.monitors(par=85128,yr1=curr.year-2,yr2=curr.year); t2$parameter <- 85128;
t3 <- get.monitors(par=88128,yr1=curr.year-2,yr2=curr.year); t3$parameter <- 88128;
pb.nonreg.monitors <- rbind(t1,t2,t3)
pm10.nonreg.monitors <- get.monitors(par=85101,yr1=curr.year-2,yr2=curr.year)
pm25.nonreg.monitors <- get.monitors(par=88502,yr1=curr.year-2,yr2=curr.year)
pm10_25.monitors <- get.monitors(par=86101,yr1=curr.year-2,yr2=curr.year)
no3 <- get.monitors(par=88306,yr1=curr.year-2,yr2=curr.year); no3$parameter <- 88306;
oc <- get.monitors(par=88320,yr1=curr.year-2,yr2=curr.year); oc$parameter <- 88320;
ec <- get.monitors(par=88321,yr1=curr.year-2,yr2=curr.year); ec$parameter <- 88321;
so4 <- get.monitors(par=88403,yr1=curr.year-2,yr2=curr.year); so4$parameter <- 88403;
pm25.spec.monitors <- rbind(no3,oc,ec,so4)
save(list=paste(c("pb.nonreg","pm10.nonreg","pm25.nonreg","pm10_25","pm25.spec"),"monitors",sep="."),
  file=paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

##################################################
## Carbon Monoxide (CO) AQS design value retrieval
##################################################
cat("Retrieving CO design values...")
## co.views <- c(aqs.tables[grep("CO_",aqs.tables)],aqs.views[grep("CO_",aqs.views)])
## co.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_CO_DVS'"))
co.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         poc AS poc, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         co_1hr_2nd_max_value AS dv_1hr,
         co_8hr_2nd_max_value AS dv_8hr
    FROM EUV_CO_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42101'
     AND state_code NOT IN ('80','CC')
ORDER BY site, poc, year"))
save(co.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/COdvs2000_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#######################################
## Lead (Pb) AQS design value retrieval
#######################################
cat("Retrieving Pb design values...")
## pb.views <- c(aqs.tables[grep("LEAD_",aqs.tables)],aqs.views[grep("LEAD_",aqs.views)])
## pb.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_LEAD_DVS'"))
pb.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         design_value AS dv,
         dv_validity_ind AS valid_dv,
         dv_year_max_value AS max_ann,
         dv_year_valid_months AS valid_ann
    FROM EUV_LEAD_DVS
   WHERE dv_year >= 2010 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '14129'
     AND pollutant_standard_id = 2
     AND dv_year_max_value IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
save(pb.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/PBdvs2010_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

####################################################
## Nitrogen Dioxide (NO2) AQS design value retrieval
####################################################
cat("Retrieving NO2 design values...")
## no2.views <- c(aqs.tables[grep("NO2",aqs.tables)],aqs.views[grep("NO2",aqs.views)])
## no2.ann.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_NO2_ANNUAL_DVS'"))
## no2.1hr.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_NO2_1HOUR_DVS'"))
no2.ann.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         design_value AS dv_ann,
         observation_percent AS pct_ann,
         dv_validity_indicator AS valid_ann
    FROM EUV_NO2_ANNUAL_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42602'
     AND pollutant_standard_id = 8
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
no2.1hr.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         dv_year AS year,
         design_value AS dv_1hr,
         year_0_98th_percentile AS p98_1hr,
         dv_validity_indicator AS valid_1hr
    FROM EUV_NO2_1HOUR_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42602'
     AND year_0_98th_percentile IS NOT NULL
     AND pollutant_standard_id = 20
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
no2.dvs <- merge(no2.ann.dvs,no2.1hr.dvs,by=c("site","year"),all=TRUE)
save(no2.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/NO2dvs2000_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

########################################
## Ozone (O3) AQS design value retrieval
########################################
cat("Retrieving O3 design values...")
## o3.views <- c(aqs.tables[grep("OZONE",aqs.tables)],aqs.views[grep("OZONE",aqs.views)])
## o3.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_OZONE_DVS'"))
o3.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         FLOOR(design_value*1000) AS dv_8hr,
         dv_3_yr_percent_complete AS pct_3yr_8hr,
         dv_validity_indicator AS valid_8hr,
         FLOOR(dv_year_4th_max) AS max4_8hr,
         dv_year_percent_complete AS pct_ann_8hr
    FROM EUV_OZONE_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '44201'
     AND standard_id = 23
     AND dv_year_4th_max IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
save(o3.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/O3dvs2000_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#################################################
## Particulates (PM10) AQS design value retrieval
#################################################
cat("Retrieving PM10 design values...")
## pm10.views <- c(aqs.tables[grep("PM10",aqs.tables)],aqs.views[grep("PM10",aqs.views)])
## pm10.conc.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'PM10_MON_DESIGN_CONCENTRATIONS'"))
## pm10.exc.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_PM10_DVS'"))
pm10.dv.conc <- get.aqs.data(paste("
  SELECT state_code || county_code || site_id AS site,
         poc AS poc,
         dv_year AS year,
         design_concentration AS dv_conc
    FROM PM10_MON_DESIGN_CONCENTRATIONS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND design_concentration IS NOT NULL
     AND parameter_code = '81102'
     AND pollutant_standard_id = 12
     AND state_code NOT IN ('80','CC')
ORDER BY site, poc, year"))
pm10.dv.exc <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site,
         poc AS poc,
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         dv_estimated_exceedances AS dv_exc,
         dv_validity_indicator AS dv_valid,
         dv_year_complete_quarters AS comp_qtrs
    FROM EUV_PM10_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '81102'
     AND pollutant_standard_id = 12
     AND dv_year_exceedance_count IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, poc, year"))
pm10.dvs <- merge(pm10.dv.exc,pm10.dv.conc,by=c("site","poc","year"))[,c(1,2,4:10,3,11:14)]
save(pm10.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/PM10dvs2000_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#######################################################
## Fine Particulates (PM2.5) AQS design value retrieval
#######################################################
cat("Retrieving PM2.5 design values...")
## pm25.views <- c(aqs.tables[grep("PM25",aqs.tables)],aqs.views[grep("PM25",aqs.views)])
## pm25.ann.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_PM25_ANNUAL_DVS'"))
## pm25.24h.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_PM25_24HR_DVS'"))
pm25.ann.dv <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         dv_year AS year,
         design_value AS dv_ann,
         dv_validity_ind AS dv_ann_valid,
         dv_year_arith_mean AS ann_mean,
         dv_year_mean_validity_ind AS ann_mean_valid
    FROM EUV_PM25_ANNUAL_DVS
   WHERE dv_year >= 2002 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '88101'
     AND pollutant_standard_id = 22
     AND dv_year_arith_mean IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"),dbname="aqsprod")
pm25.24h.dv <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site,
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         daily_design_value AS dv_24h,
         dv_validity_ind AS dv_24h_valid,
         dv_year_98th_percentile AS ann_p98,
         dv_year_98th_validity_ind AS ann_p98_valid
    FROM EUV_PM25_24HR_DVS
   WHERE dv_year >= 2002 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '88101'
     AND pollutant_standard_id = 21
     AND dv_year_98th_percentile IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"),dbname="aqsprod")
pm25.dvs <- merge(pm25.24h.dv,pm25.ann.dv,by=c("site","year"),all=TRUE)[,c(1,3:9,2,10:17)]
save(pm25.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/PM25dvs2002_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

##################################################
## Sulfur Dioxide (SO2) AQS design value retrieval
##################################################
cat("Retrieving SO2 design values...")
## so2.views <- c(aqs.tables[grep("SO2",aqs.tables)],aqs.views[grep("SO2",aqs.views)])
## so2.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'EUV_SO2_DVS'"))
so2.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         design_value AS dv,
         dv_validity_indicator AS dv_valid,
         year_0_99th_percentile AS p99,
         year_0_complete_ind AS p99_valid
    FROM EUV_SO2_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42401'
     AND pollutant_standard_id = 19
     AND year_0_99th_percentile IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
save(so2.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/SO2dvs2000_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

######################################
## AQS annual summary retrieval for CO
######################################
cat("Retrieving annual CO values...")
co.annual.1hr <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sm.max_sample_value AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_maximums sm
   WHERE sm.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1980
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id = 3
     AND ans.sd_duration_code = '1'
     AND mo.pa_parameter_code = '42101'
     AND si.state_code != '80'
     AND sm.max_level = 2
ORDER BY 1,2,3",sep=""))
co.annual.8hr <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sm.max_sample_value AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_maximums sm
   WHERE sm.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1980
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id = 4
     AND ans.sd_duration_code = 'Z'
     AND mo.pa_parameter_code = '42101'
     AND si.state_code != '80'
     AND sm.max_ind = 'OVR'
     AND sm.max_level = 2
ORDER BY 1,2,3",sep=""))
co.annual.all <- na.omit(merge(co.annual.1hr,co.annual.8hr,by=c("site","poc","year"),
  all=TRUE,suffixes=c(".1hr",".8hr")))
co.annual <- ddply(co.annual.all,c("site","year"),summarize,
  conc.1hr=max(conc.1hr),conc.8hr=max(conc.8hr))
save(co.annual,file=paste("NAAQS_AQ/data/",curr.year,"/COannual1980_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#############################################################################
## AQS monthly mean data retrieval and annual 3-month max calculations for Pb
#############################################################################
cat("Retrieving annual Pb values...")
pb.monthly <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         pb.parameter_code AS par,
         pb.summary_year AS year,
         pb.summary_month AS month,
         pb.total_sampled_day_count AS obs,
         pb.arith_mean AS conc
    FROM lead_site_monthly_summaries pb,
         site_basic si
   WHERE pb.si_id = si.si_id
     AND pb.arith_mean IS NOT NULL
     AND pb.edt_id IN (0,2)
     AND pb.summary_year >= 1979
     AND pb.summary_year <= ",curr.year,"
     AND si.state_code != '80'
ORDER BY 1,2,3,4",sep=""))
pb.3month <- data.frame(pb.monthly[,1:4],obs=NA,conc=NA)
for (i in 3:nrow(pb.3month)) {
  if (pb.monthly$site[i-1] != pb.monthly$site[i] | pb.monthly$site[i-2] != pb.monthly$site[i]) { next }
  if (pb.monthly$par[i-1] != pb.monthly$par[i] | pb.monthly$par[i-2] != pb.monthly$par[i]) { next }
  pb.3month$obs[i] <- pb.monthly$obs[i-2] + pb.monthly$obs[i-1] + pb.monthly$obs[i]
  pb.3month$conc[i] <- (pb.monthly$conc[(i-2):i] %*% pb.monthly$obs[(i-2):i])/sum(pb.monthly$obs[(i-2):i])
}
pb.3month <- subset(pb.3month,!is.na(conc) & obs >= 11 & year > 1979)
pb.annual <- ddply(pb.3month,c("site","year"),summarize,N=length(conc),conc=max(conc))
save(list=c("pb.annual","pb.monthly"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PBannual1980_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#######################################
## AQS annual summary retrieval for NO2
#######################################
cat("Retrieving annual NO2 values...")
no2.annual.1hr <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sp.percentile_sample_value AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_percentiles sp
   WHERE sp.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1980
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id = 20
     AND ans.sd_duration_code = '1'
     AND mo.pa_parameter_code = '42602'
     AND si.state_code != '80'
     AND sp.percentile_num = 98
ORDER BY 1,2,3",sep=""))
no2.annual.mean <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         ans.annual_arith_mean AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si
   WHERE ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_arith_mean IS NOT NULL
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1980
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id = 8
     AND ans.sd_duration_code = '1'
     AND mo.pa_parameter_code = '42602'
     AND si.state_code != '80'
ORDER BY 1,2,3",sep=""))
no2.annual.all <- na.omit(merge(no2.annual.1hr,no2.annual.mean,by=c("site","poc","year"),
  all=TRUE,suffixes=c(".1hr",".ann")))
no2.annual <- ddply(no2.annual.all,c("site","year"),summarize,
  conc.1hr=max(conc.1hr),conc.ann=round(max(conc.ann),2))
save(no2.annual,file=paste("NAAQS_AQ/data/",curr.year,"/NO2annual1980_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

######################################
## AQS annual summary retrieval for O3
######################################
cat("Retrieving annual O3 values...")
o3.annual.8hr <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sm.max_sample_value*1000 AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_maximums sm
   WHERE sm.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1980
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id = 23
     AND ans.sd_duration_code = 'W'
     AND mo.pa_parameter_code = '44201'
     AND si.state_code != '80'
     AND sm.max_level = 4
ORDER BY 1,2,3",sep=""))
o3.annual <- ddply(o3.annual.8hr,c("site","year"),summarize,conc=max(conc))
save(o3.annual,file=paste("NAAQS_AQ/data/",curr.year,"/O3annual1980_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

########################################
## AQS annual summary retrieval for PM10
########################################
cat("Retrieving annual PM10 values...")
pm10.annual.24h <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.pa_parameter_code AS par,         
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sm.max_sample_value AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_maximums sm
   WHERE sm.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1990
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id IN (0,12)
     AND ans.sd_duration_code IN ('7','X')
     AND mo.pa_parameter_code IN ('81102','85101')
     AND si.state_code != '80'
     AND sm.max_level = 2
ORDER BY 1,2,3,4",sep=""))
pm10.annual <- ddply(pm10.annual.24h,c("site","year"),summarize,conc=conc[1])
save(pm10.annual,file=paste("NAAQS_AQ/data/",curr.year,"/PM10annual1990_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#########################################
## AQS annual summary retrieval for PM2.5
#########################################
cat("Retrieving annual PM2.5 values...")
pm25.annual.mean <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.pa_parameter_code AS par,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         ans.annual_arith_mean AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si
   WHERE ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_arith_mean IS NOT NULL
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 2000
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id IN (0,22)
     AND ans.sd_duration_code IN ('7','X')
     AND mo.pa_parameter_code IN ('88101','88502')
     AND si.state_code != '80'
ORDER BY 1,2,3",sep=""))
pm25.annual.24h <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.pa_parameter_code AS par,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sp.percentile_sample_value AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_percentiles sp
   WHERE sp.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_arith_mean IS NOT NULL
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 2000
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id IN (0,21)
     AND ans.sd_duration_code IN ('7','X')
     AND mo.pa_parameter_code IN ('88101','88502')
     AND si.state_code != '80'
     AND sp.percentile_num = 98
ORDER BY 1,2,3,4",sep=""))
pm25.annual.all <- na.omit(merge(pm25.annual.mean,pm25.annual.24h,by=c("site","par","poc","year"),
  all=TRUE,suffixes=c(".ann",".24h")))
pm25.annual <- ddply(pm25.annual.all,c("site","year"),summarize,
  conc.ann=round(conc.ann[1],2),conc.24h=conc.24h[1])
save(pm25.annual,file=paste("NAAQS_AQ/data/",curr.year,"/PM25annual2000_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

#######################################
## AQS annual summary retrieval for SO2
#######################################
cat("Retrieving annual SO2 values...")
so2.annual.1hr <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         ans.annual_summary_year AS year,
         sp.percentile_sample_value AS conc
    FROM annual_summaries ans,
         monitors mo,
         site_basic si,
         summary_percentiles sp
   WHERE sp.ans_ans_id = ans.ans_id
     AND ans.mo_mo_id = mo.mo_id
     AND mo.si_si_id = si.si_id
     AND ans.annual_obs_pct >= 75
     AND ans.annual_summary_year >= 1980
     AND ans.annual_summary_year <= ",curr.year,"
     AND ans.edt_edt_id IN (0,2)
     AND ans.pollutant_standard_id = 19
     AND ans.sd_duration_code = '1'
     AND mo.pa_parameter_code = '42401'
     AND si.state_code != '80'
     AND sp.percentile_num = 99
ORDER BY 1,2,3",sep=""))
so2.annual <- ddply(so2.annual.1hr,c("site","year"),summarize,conc=max(conc))
save(so2.annual,file=paste("NAAQS_AQ/data/",curr.year,"/SO2annual1980_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

####################################################################
## AQS annual summary data retrieval for coarse PM and PM2.5 species
####################################################################
cat("Retrieving annual coarse PM and PM2.5 species values...")
annual.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'ANNUAL_SUMMARIES'"))
get.annual.data <- function(par,name,years) {
  ny <- length(years)
  vals <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         si.standard_latitude AS latitude,
         si.standard_longitude AS longitude,
         ans.annual_summary_year AS year,
         ans.annual_arith_mean AS ",name,"
    FROM annual_summaries ans,
         monitors mo,
         site_basic si
   WHERE ans.annual_summary_year >= ",years[1],"
     AND ans.annual_summary_year <= ",years[ny],"
     AND ans.annual_obs_cnt >= 30
     AND ans.edt_edt_id IN (0,2)
     AND ans.mo_mo_id = mo.mo_id
     AND ans.pollutant_standard_id = 0
     AND ans.sd_duration_code IN ('1','7')
     AND mo.pa_parameter_code = '",par,"'
     AND mo.si_si_id = si.si_id
     AND si.state_code NOT IN ('80','CC')
   ORDER BY 1,2,3",sep=""))
 return(vals)
}
pm10_25 <- get.annual.data(par=86101,name="pm10_25",years=c(2005:curr.year))
no3 <- get.annual.data(par=88306,name="no3",years=c(2002:curr.year))
oc1 <- get.annual.data(par=88320,name="oc",years=c(2002:curr.year))
oc2 <- get.annual.data(par=88370,name="oc",years=c(2002:curr.year))
oc <- rbind(oc1,oc2); oc <- oc[order(oc$site,oc$poc,oc$year),];
ec1 <- get.annual.data(par=88321,name="ec",years=c(2002:curr.year))
ec2 <- get.annual.data(par=88380,name="ec",years=c(2002:curr.year))
ec <- rbind(ec1,ec2); ec <- ec[order(ec$site,ec$poc,ec$year),];
so4 <- get.annual.data(par=88403,name="so4",years=c(2002:curr.year))
al <- get.annual.data(par=88104,name="al",years=c(2002:curr.year))
ca <- get.annual.data(par=88111,name="ca",years=c(2002:curr.year))
fe <- get.annual.data(par=88126,name="fe",years=c(2002:curr.year))
si <- get.annual.data(par=88165,name="si",years=c(2002:curr.year))
ti <- get.annual.data(par=88161,name="ti",years=c(2002:curr.year))
cl1 <- get.annual.data(par=88115,name="cl1",years=c(2002:curr.year))
cl2 <- get.annual.data(par=88203,name="cl2",years=c(2002:curr.year))
pm25.spec <- Reduce(merge.all,lapply(c("no3","oc","ec","so4","al","ca","fe","si","ti","cl1","cl2"),get))
pm25.spec$crustal <- 2.2*pm25.spec$al+1.63*pm25.spec$ca+2.42*pm25.spec$fe+2.49*pm25.spec$si+1.94*pm25.spec$ti
pm25.spec$seasalt <- 1.8*mapply(pmax.na,pm25.spec$cl1,pm25.spec$cl2)
save(list=c("pm10_25","pm25.spec"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM25spec_annual2002_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

##############################################################################
## AQS daily summary data retrieval for all criteria pollutants and PM species
##############################################################################
## daily.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'DAILY_SUMMARIES'"))
get.daily.data <- function(par,psid,dur,stat,years) {
  ds.stat <- switch(stat,max="ds.daily_max_sample_value",mean="ds.daily_arith_mean")
  ny <- length(years)
  begin.date <- paste(years[1]-ifelse(par == "14129",1,0),
    ifelse(par == "14129","11-01","01-01"),sep="-")
  end.date <- paste(years[ny],"12-31",sep="-")
  vals <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id || mo.poc AS id,
         ds.daily_coll_date AS dt,
         ",ds.stat," AS conc
    FROM daily_summaries ds,
         monitors mo,
         site_basic si
   WHERE ds.daily_coll_date >= TO_DATE('",begin.date,"','YYYY-MM-DD')
     AND ds.daily_coll_date <= TO_DATE('",end.date,"','YYYY-MM-DD')
     AND ds.edt_edt_id IN (0,2)
     AND ds.mo_mo_id = mo.mo_id
     AND ds.parameter_code = '",par,"'
     AND ds.pollutant_standard_id = ",psid,"
     AND ds.sd_duration_code = '",dur,"'
     AND mo.pa_parameter_code = '",par,"'
     AND mo.si_si_id = si.si_id
     AND si.state_code NOT IN ('80','CC')
   ORDER BY 1,2",sep=""))
   all <- data.frame(id=rep(unique(vals$id),each=length(unique(vals$dt))),
     dt=rep(unique(vals$dt),times=length(unique(vals$id))))
   data <- merge(all,vals,by=c("id","dt"),all=TRUE)
   daily <- data.frame(site=substr(data$id,1,9),poc=substr(data$id,10,10),
     date=as.Date(data$dt),conc=data$conc)
   return(daily)
}
curr.3yr <- c((curr.year-2):curr.year)
file.yrs <- paste(curr.3yr[1],curr.3yr[3],sep="_")

## Daily CO data retrieval
cat("Retrieving daily CO data...")
co.mda1 <- get.daily.data(par="42101",psid=3,dur="1",stat="max",years=curr.3yr)
co.mda8 <- get.daily.data(par="42101",psid=4,dur="Z",stat="max",years=curr.3yr)
co.mean <- get.daily.data(par="42101",psid=3,dur="1",stat="mean",years=curr.3yr)
co.daily <- merge(merge(co.mda1,co.mda8,by=c("site","poc","date"),all=TRUE,
   suffixes=c(".mda1",".mda8")),co.mean,by=c("site","poc","date"),all=TRUE)
save(co.daily,file=paste("NAAQS_AQ/data/",curr.year,"/COdaily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily Pb data retrieval
cat("Retrieving daily Pb data...")
pb.tsp1 <- get.daily.data(par="12128",psid=1,dur="7",stat="mean",years=curr.3yr); pb.tsp1$parameter <- "12128";
pb.tsp2 <- get.daily.data(par="14129",psid=2,dur="7",stat="mean",years=curr.3yr); pb.tsp2$parameter <- "14129";
pb.pm10.1 <- get.daily.data(par="82128",psid=0,dur="7",stat="mean",years=curr.3yr); pb.pm10.1$parameter <- "82128";
pb.pm10.2 <- get.daily.data(par="85128",psid=0,dur="7",stat="mean",years=curr.3yr); pb.pm10.2$parameter <- "85128";
pb.pm25 <- get.daily.data(par="88128",psid=0,dur="7",stat="mean",years=curr.3yr); pb.pm25$parameter <- "88128";
pb.daily <- rbind(pb.tsp1,pb.tsp2,pb.pm10.1,pb.pm10.2,pb.pm25);
save(pb.daily,file=paste("NAAQS_AQ/data/",curr.year,"/PBdaily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily NO2 data retrieval
cat("Retrieving daily NO2 data...")
no2.max <- get.daily.data(par="42602",psid=20,dur="1",stat="max",years=curr.3yr)
no2.mean <- get.daily.data(par="42602",psid=8,dur="1",stat="mean",years=curr.3yr)
no2.daily <- merge(no2.max,no2.mean,by=c("site","poc","date"),all=TRUE,suffixes=c(".max",".mean"))
save(no2.daily,file=paste("NAAQS_AQ/data/",curr.year,"/NO2daily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily O3 data retrieval
cat("Retrieving daily O3 data...")
o3.mda1 <- get.daily.data(par="44201",psid=9,dur="1",stat="max",years=curr.3yr)
o3.mda8 <- get.daily.data(par="44201",psid=23,dur="W",stat="max",years=curr.3yr)
o3.mean <- get.daily.data(par="44201",psid=9,dur="1",stat="mean",years=curr.3yr)
o3.daily <- merge(merge(o3.mda1,o3.mda8,by=c("site","poc","date"),all=TRUE,
   suffixes=c(".mda1",".mda8")),o3.mean,by=c("site","poc","date"),all=TRUE)
colnames(o3.daily)[ncol(o3.daily)] <- "conc.mean"; o3.daily[,4:6] <- o3.daily[,4:6]*1000;
save(o3.daily,file=paste("NAAQS_AQ/data/",curr.year,"/O3daily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily PM10 data retrieval
cat("Retrieving daily PM10 data...")
pm10.filt.1 <- get.daily.data(par="81102",psid=12,dur="7",stat="mean",years=curr.3yr); pm10.filt.1$parameter <- "81102";
pm10.filt.2 <- get.daily.data(par="85101",psid=0,dur="7",stat="mean",years=curr.3yr); pm10.filt.2$parameter <- "85101";
pm10.cont.1 <- get.daily.data(par="81102",psid=12,dur="X",stat="mean",years=curr.3yr); pm10.cont.1$parameter <- "81102";
pm10.cont.2 <- get.daily.data(par="85101",psid=0,dur="1",stat="mean",years=curr.3yr); pm10.cont.2$parameter <- "85101";
pm10.dmax.1 <- get.daily.data(par="81102",psid=0,dur="1",stat="max",years=curr.3yr); pm10.dmax.1$parameter <- "81102";
pm10.dmax.2 <- get.daily.data(par="85101",psid=0,dur="1",stat="max",years=curr.3yr); pm10.dmax.2$parameter <- "85101";
pm10.daily <- rbind(pm10.filt.1,pm10.cont.1,pm10.filt.2,pm10.cont.2)
pm10.dmax <- rbind(pm10.dmax.1,pm10.dmax.2)
pm10.daily <- pm10.daily[order(pm10.daily$site,pm10.daily$poc,pm10.daily$date),]
pm10.dmax <- pm10.dmax[order(pm10.dmax$site,pm10.dmax$poc,pm10.dmax$date),]
save(list=c("pm10.daily","pm10.dmax"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM10daily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily PM2.5 data retrieval
cat("Retrieving daily PM2.5 data...")
pm25.filt.1 <- get.daily.data(par="88101",psid=21,dur="7",stat="mean",years=curr.3yr); pm25.filt.1$parameter <- "88101";
pm25.filt.2 <- get.daily.data(par="88502",psid=0,dur="7",stat="mean",years=curr.3yr); pm25.filt.2$parameter <- "88502";
pm25.cont.1 <- get.daily.data(par="88101",psid=21,dur="X",stat="mean",years=curr.3yr); pm25.cont.1$parameter <- "88101";
pm25.cont.2 <- get.daily.data(par="88502",psid=0,dur="X",stat="mean",years=curr.3yr); pm25.cont.2$parameter <- "88502";
pm25.dmax.1 <- get.daily.data(par="88101",psid=0,dur="1",stat="max",years=curr.3yr); pm25.dmax.1$parameter <- "88101";
pm25.dmax.2 <- get.daily.data(par="88502",psid=0,dur="1",stat="max",years=curr.3yr); pm25.dmax.2$parameter <- "88502";
pm25.daily <- rbind(pm25.filt.1,pm25.cont.1,pm25.filt.2,pm25.cont.2)
pm25.dmax <- rbind(pm25.dmax.1,pm25.dmax.2)
pm25.daily <- pm25.daily[order(pm25.daily$site,pm25.daily$poc,pm25.daily$date),]
pm25.dmax <- pm25.dmax[order(pm25.dmax$site,pm25.dmax$poc,pm25.dmax$date),]
save(list=c("pm25.daily","pm25.dmax"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM25daily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily SO2 data retrieval
cat("Retrieving daily SO2 data...")
so2.max <- get.daily.data(par="42401",psid=19,dur="1",stat="max",years=curr.3yr)
so2.mean <- get.daily.data(par="42401",psid=19,dur="1",stat="mean",years=curr.3yr)
so2.daily <- merge(so2.max,so2.mean,by=c("site","poc","date"),all=TRUE,suffixes=c(".max",".mean"))
save(so2.daily,file=paste("NAAQS_AQ/data/",curr.year,"/SO2daily",file.yrs,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

## Daily coarse PM and PM2.5 species data retrieval
cat("Retrieving daily coarse PM and PM2.5 species data...")
pm10_25.filt <- get.daily.data(par="86101",psid=0,dur="7",stat="mean",years=curr.3yr)
pm10_25.cont <- get.daily.data(par="86101",psid=0,dur="1",stat="mean",years=curr.3yr)
pm10_25.daily <- rbind(pm10_25.filt,pm10_25.cont)
pm10_25.dmax <- get.daily.data(par="86101",psid=0,dur="1",stat="max",years=curr.3yr)
no3 <- get.daily.data(par=88306,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(no3)[4] <- "no3";
oc <- get.daily.data(par=88320,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(oc)[4] <- "oc";
ec <- get.daily.data(par=88321,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(ec)[4] <- "ec";
so4 <- get.daily.data(par=88403,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(so4)[4] <- "so4";
al <- get.daily.data(par=88104,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(al)[4] <- "al";
ca <- get.daily.data(par=88111,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(ca)[4] <- "ca";
fe <- get.daily.data(par=88126,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(fe)[4] <- "fe";
si <- get.daily.data(par=88165,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(si)[4] <- "si";
ti <- get.daily.data(par=88161,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(ti)[4] <- "ti";
cl1 <- get.daily.data(par=88115,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(cl1)[4] <- "cl1";
cl2 <- get.daily.data(par=88203,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(cl2)[4] <- "cl2";
pm25.spec <- Reduce(merge.all,lapply(c("no3","oc","ec","so4","al","ca","fe","si","ti","cl1","cl2"),get))
pm25.spec$crustal <- 2.2*pm25.spec$al+1.63*pm25.spec$ca+2.42*pm25.spec$fe+2.49*pm25.spec$si+1.94*pm25.spec$ti
pm25.spec$seasalt <- 1.8*mapply(pmax.na,pm25.spec$cl1,pm25.spec$cl2)
obs <- apply(pm25.spec,1,function(x) sum(!is.na(x)))
pm25.spec.daily <- subset(pm25.spec,obs > 3)
save(list=c("pm10_25.daily","pm10_25.dmax","pm25.spec.daily"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM25spec_daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
cat("Done.",as.character(Sys.time()),"\n")

########################################################################
## AQS hourly data retrieval for gaseous pollutants (not currently used)
########################################################################
## hourly.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns WHERE table_name = 'RAW_DATA_CONCURRENCES'"))
get.hourly.data <- function(par,year) {
  poll.name <- switch(paste("p",par,sep=""),p14129="pb",p42101="co",
    p42401="so2",p42602="no2",p44201="o3",p81102="pm10",p88101="pm25")
  dt.begin <- paste(year,"01-01 00:00:00",sep="-")
  dt.end <- paste(year,"12-31 23:00:00",sep="-")
  aqs.data <- subset(get.aqs.data(paste("SELECT DISTINCT
          rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
          TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
          GREATEST(rd.standard_sample_value,0) AS ",poll.name,",
          rd.method_code AS method,
          COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
          COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
     FROM raw_data_concurrences rd
    WHERE rd.duration_code = '1'
      AND rd.parameter_code = '",par,"'
      AND rd.sampling_begin_datetime >= TO_DATE('",dt.begin,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.sampling_begin_datetime <= TO_DATE('",dt.end,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.state_code NOT IN ('80','CC')
    ORDER BY 1,2",sep="")),substr(dt,15,19) == "00:00")
  if (par == 14129) { aqs.data$pb <- floor(100*aqs.data$pb)/100 }
  if (par == 42101) { aqs.data$co <- floor(10*aqs.data$co)/10 }
  if (par == 42401) { aqs.data$so2 <- floor(10*aqs.data$so2)/10 }
  if (par == 42602) { aqs.data$no2 <- floor(10*aqs.data$no2)/10 }
  if (par == 44201) { aqs.data$o3 <- floor(aqs.data$o3*1000) }
  if (par == 81102) { aqs.data$pm10 <- floor(10*aqs.data$pm10)/10 }
  if (par == 88101) { aqs.data$pm25 <- floor(10*aqs.data$pm25)/10 }
  all.ids <- as.character(unique(aqs.data$id))
  all.dts <- unique(aqs.data$dt)[order(unique(aqs.data$dt))]
  all.hrs <- data.frame(id=rep(all.ids,each=length(all.dts)),
                        dt=rep(all.dts,times=length(all.ids)))
  out <- merge(all.hrs,aqs.data,all.x=TRUE,all.y=FALSE)
  out$method <- replace(out$method,which(is.na(out$method))," ")
  out$flag <- replace(out$flag,which(is.na(out$flag))," ")
  out$concur <- replace(out$concur,which(is.na(out$concur))," ")
  assign(poll.name,out)
  file.out <- paste("NAAQS_AQ/data/",curr.year,"/",toupper(poll.name),"hourly",year,".Rdata",sep="")
  save(list=poll.name,file=file.out)
}
##for (p in c('42101','42401','42602','44201')) {
##  for (y in c((curr.year-2):curr.year)) {
##    get.hourly.data(par=p,year=y)
##    cat(p,y,as.character(Sys.time()),"\n")
##  }
##}
cat("AQS data retrievals completed.",as.character(Sys.time()),"\n\n")