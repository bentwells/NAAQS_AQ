###############################################
## Generate map with PM2.5 speciation piecharts
###############################################

## Set up working environment
cat("Generating .png image of PM2.5 speciation piecharts... ")
pie.colors <- c("#FFFF00","#FF0000","#00FFFF","#000000","#A52A2A","#0000FF")

## Load daily speciation data for most recent 3 years, calculate average percent values
load(paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/PM25spec_daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
daily <- na.omit(ddply(pm25.spec.daily,c("site","date"),summarize,no3=max.na(no3),oc=max.na(oc),
  ec=max.na(ec),so4=max.na(so4),crustal=max.na(crustal),seasalt=max.na(seasalt)))
daily$total <- apply(daily[-c(1:2)],1,sum)
vals <- subset(ddply(daily,"site",summarize,obs=length(date),total=mean(total),no3=mean(no3),
  oc=mean(oc),ec=mean(ec),so4=mean(so4),crustal=mean(crustal),seasalt=mean(seasalt)),obs >= 200)
pcts <- data.frame(site=vals$site,t(apply(vals[,-c(1:3)],1,function(x) round(100*x/sum(x),1))))
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
pm25.spec.monitors$type <- sapply(pm25.spec.monitors$network,function(x) 
  ifelse(grepl("CSN",x),"CSN",ifelse(x == "IMPROVE","IMPROVE","OTHER")))
pm25.spec.monitors <- pm25.spec.monitors[order(pm25.spec.monitors$site,pm25.spec.monitors$type),]
sites <- subset(pm25.spec.monitors,!duplicated(site),c("site","site_name","state_name","county_name",
  "cbsa_name","type","latitude","longitude"))
map.vals <- merge(sites,merge(vals[,c("site","total")],pcts,by="site"),by="site")
map.vals <- map.vals[order(map.vals$total,decreasing=TRUE),]

## Generate map with PM2.5 speciation piecharts for each location
xy <- add.layer(type="points",x=map.vals$longitude,y=map.vals$latitude,plot=FALSE,proj.args=pa)
d <- rdist(xy); map.vals$x <- xy$x; map.vals$y <- xy$y;
n <- 1; nr <- nrow(map.vals); 
while(n <= nr) {
  cut <- 0.02
  rm <- which(d[,n] > 0 & d[,n] < cut)
  if (length(rm) > 0) {
    map.vals <- map.vals[-rm,]
    nr <- nr - length(rm)
    d <- d[-rm,-rm]
  }
  n <- n + 1
}

file.name <- paste("NAAQS_AQ/spec_piecharts/speciation_piecharts_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1800,height=1200,quality=100)
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(map.vals)) {
  pie.chart(x=unlist(map.vals[i,c("so4","no3","oc","ec","crustal","seasalt")]),labels="",
    radius=0.01,add=TRUE,x.ctr=map.vals$x[i],y.ctr=map.vals$y[i],col=pie.colors)
}
pie.chart(x=rep(1,6),labels=c("Sulfates","Nitrates","OC","EC","Crustal","Sea Salt"),
  radius=0.02,add=TRUE,x.ctr=0.35,y.ctr=-1.35,col=pie.colors,cex=1.5)
dev.off()
cat("Done.",as.character(round(Sys.time())),"\n")