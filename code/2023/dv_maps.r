####################################################
## Generate design value maps for NAAQS AQ documents
####################################################

## Set up working environment
cat("Creating design value maps .png images... ")
if (!dir.exists(paste("NAAQS_AQ/dv_maps",curr.year,sep="/"))) { 
  dir.create(paste("NAAQS_AQ/dv_maps",curr.year,sep="/")) 
}

## CO design value maps
load(paste("NAAQS_AQ/data/",curr.year,"/COdvs2000_",curr.year,".Rdata",sep=""))
dvs.co.1hr <- ddply(subset(co.dvs,year == curr.year & !is.na(dv_1hr)),c("site"),summarize,
  latitude=latitude[1],longitude=longitude[1],year=year[1],dv_1hr=max(dv_1hr))
vals <- dvs.co.1hr$dv_1hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(5.1,10.1,20.1,35.1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-0.1,Inf))); keep <- which(N > 0);
legend.lab <- paste(sprintf("%4.1f",bin.min)," - ",bin.max," ppm (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/COdvmap1hr",curr.year-1,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.co.1hr$longitude[o],y=dvs.co.1hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

dvs.co.8hr <- ddply(subset(co.dvs,year == curr.year & !is.na(dv_8hr)),c("site"),summarize,
  latitude=latitude[1],longitude=longitude[1],year=year[1],dv_8hr=max(dv_8hr))
vals <- dvs.co.8hr$dv_8hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(3.1,6.1,9.1,15.1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-0.1,Inf))); keep <- which(N > 0);
legend.lab <- paste(sprintf("%4.1f",bin.min)," - ",bin.max," ppm (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/COdvmap8hr",curr.year-1,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.co.8hr$longitude[o],y=dvs.co.8hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## NO2 design value maps
load(paste("NAAQS_AQ/data/",curr.year,"/NO2dvs2000_",curr.year,".Rdata",sep=""))
dvs.no2.1hr <- subset(no2.dvs,year == curr.year & valid_1hr == "Y",
  c("site","latitude","longitude","year","dv_1hr"))
vals <- dvs.no2.1hr$dv_1hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(26,51,76,101); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/NO2dvmap1hr",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.no2.1hr$longitude[o],y=dvs.no2.1hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=4)
dev.off()

dvs.no2.ann <- subset(no2.dvs,year == curr.year & !is.na(dv_ann) & valid_ann == "Y",
  c("site","latitude","longitude","year","dv_ann"))
vals <- round(dvs.no2.ann$dv_ann); o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(11,21,31,54); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/NO2dvmapann",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.no2.ann$longitude[o],y=dvs.no2.ann$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## O3 design value map
load(paste("NAAQS_AQ/data/",curr.year,"/O3dvs2000_",curr.year,".Rdata",sep=""))
dvs.o3.8hr <- subset(o3.dvs,year == curr.year & valid_8hr == "Y",
  c("site","latitude","longitude","year","dv_8hr"))
vals <- dvs.o3.8hr$dv_8hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(61,66,71,76,85); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/O3dvmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.o3.8hr$longitude[o],y=dvs.o3.8hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Pb design value map
load(paste("NAAQS_AQ/data/",curr.year,"/PBdvs2010_",curr.year,".Rdata",sep=""))
dvs.pb.3mo <- subset(pb.dvs,year == curr.year & valid_dv == "Y",
  c("site","latitude","longitude","year","dv"))
vals <- dvs.pb.3mo$dv; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.06,0.11,0.16,0.21); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.01,max(vals));
N <- table(cut(vals,breaks=c(0,bins-0.01,Inf))); keep <- which(N > 0);
legend.lab <- paste(sprintf("%5.2f",bin.min)," - ",sprintf("%5.2f",bin.max)," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/Pbdvmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pb.3mo$longitude[o],y=dvs.pb.3mo$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## PM10 design value map
load(paste("NAAQS_AQ/data/",curr.year,"/PM10dvs2000_",curr.year,".Rdata",sep=""))
dvs.pm10.24hr <- subset(pm10.dvs,year == curr.year & dv_valid == "Y",
  c("site","latitude","longitude","year","dv_conc"))
vals <- dvs.pm10.24hr$dv_conc; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(51,101,151,201,501); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM10dvmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pm10.24hr$longitude[o],y=dvs.pm10.24hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## PM2.5 design value maps
load(paste("NAAQS_AQ/data/",curr.year,"/PM25dvs2002_",curr.year,".Rdata",sep=""))
dvs.pm25.24hr <- subset(pm25.dvs,year == curr.year & dv_24h_valid == "Y",
  c("site","latitude","longitude","year","dv_24h"))
vals <- dvs.pm25.24hr$dv_24h; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(16,26,36,51,101); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM25dvmap24h",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pm25.24hr$longitude[o],y=dvs.pm25.24hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

dvs.pm25.ann <- subset(pm25.dvs,year == curr.year & dv_ann_valid == "Y",
  c("site","latitude","longitude","year","dv_ann"))
vals <- dvs.pm25.ann$dv_ann; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(5.1,7.1,9.1,12.1,15.1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-0.1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",sprintf("%4.1f",bin.max)," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM25dvmapann",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pm25.ann$longitude[o],y=dvs.pm25.ann$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## SO2 design value map
load(paste("NAAQS_AQ/data/",curr.year,"/SO2dvs2000_",curr.year,".Rdata",sep=""))
dvs.so2.1hr <- subset(so2.dvs,year == curr.year & dv_valid == "Y",
  c("site","latitude","longitude","year","dv"))
vals <- dvs.so2.1hr$dv; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(26,51,76,101,251); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
N <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/SO2dvmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.so2.1hr$longitude[o],y=dvs.so2.1hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

###############################################################################
## Annual average concentration maps for coarse PM (PM10-2.5) and PM2.5 species
###############################################################################
load(paste("NAAQS_AQ/data/",curr.year,"/PM25spec_annual2002_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
pm10_25.monitors$site <- substr(pm10_25.monitors$id,1,9)
pm10_25.sites <- subset(pm10_25.monitors,!duplicated(site),c("site","latitude","longitude"))
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
pm25.spec.sites <- subset(pm25.spec.monitors,!duplicated(site),c("site","latitude","longitude"))

## Annual average coarse PM concentration map
t <- subset(ddply(pm10_25,c("site","year"),summarize,pm10_25=max.na(pm10_25)),year >= curr.year-2)
map.vals <- merge(pm10_25.sites,ddply(t,c("site"),summarize,pm10_25=mean(pm10_25)))
vals <- map.vals$pm10_25; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(2,4,6,10); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.1,round(max(vals)));
N <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM10_25avgmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average sulfate (SO4) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,so4=max.na(so4)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,so4=mean(so4))))
vals <- map.vals$so4; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.5,1,1.5,2); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
N <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM25_SO4avgmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average nitrate (NO3) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,no3=max.na(no3)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,no3=mean(no3))))
vals <- map.vals$no3; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.5,1,1.5,2); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
N <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM25_NO3avgmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average elemental carbon (EC) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,ec=max.na(ec)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,ec=mean(ec))))
vals <- map.vals$ec; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.25,0.5,0.75,1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
N <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM25_ECavgmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average organic carbon (OC) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,oc=max.na(oc)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,oc=mean(oc))))
vals <- map.vals$oc; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(1,2,3,4); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
N <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(N > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",N," sites)",sep="")
if (any(N == 1)) { legend.lab[which(N == 1)] <- gsub("sites","site",legend.lab[which(N == 1)]) }
file.name <- paste("NAAQS_AQ/dv_maps/",curr.year,"/PM25_OCavgmap",curr.year-2,"_",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()
cat("Done.",as.character(round(Sys.time())),"\n")
