###############################################
## Generate monitor maps for NAAQS AQ documents
###############################################

## Set up working environment
cat("Creating .png images of monitor maps... ")
colors <- c("gray50","blue3","yellow3","red3","black")
load(paste("NAAQS_AQ/data/",curr.year,"/monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
if (!dir.exists(paste("NAAQS_AQ/monitor_maps",curr.year,sep="/"))) { 
  dir.create(paste("NAAQS_AQ/monitor_maps",curr.year,sep="/")) 
}

## CO monitor map
co.monitors$site <- substr(co.monitors$id,1,9)
co.monitors$class <- mapply(function(type,network) ifelse(network == "NEAR ROAD","NEAR ROAD",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS",ifelse(type == "TRIBAL",
  "TRIBAL","SPM/OTHER")))),co.monitors$monitor_type,co.monitors$network)
t <- ddply(co.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  NE=colors[3],NC=colors[2],SL=colors[1],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,NE=23,NC=22,SL=21,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,1,2,4,5)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/COmonitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
  pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## NO2 monitor map
no2.monitors$site <- substr(no2.monitors$id,1,9)
no2.monitors$class <- mapply(function(type,network) ifelse(network == "NEAR ROAD","NEAR ROAD",
  ifelse(network %in% c("NCORE","PROPOSED NCORE","PAMS","UNOFFICIAL PAMS"),"NCORE/PAMS",
  ifelse(type == "SLAMS","SLAMS",ifelse(type == "TRIBAL","TRIBAL","SPM/OTHER")))),
  no2.monitors$monitor_type,no2.monitors$network)
t <- ddply(no2.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  NE=colors[3],NC=colors[2],SL=colors[1],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,NE=23,NC=22,SL=21,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,1,2,4,5)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/NO2monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## O3 monitor map
o3.monitors$site <- substr(o3.monitors$id,1,9)
o3.monitors$class <- mapply(function(type,network) ifelse(network == "CASTNET","CASTNET",
  ifelse(network %in% c("NCORE","PROPOSED NCORE","PAMS","UNOFFICIAL PAMS"),"NCORE/PAMS",
  ifelse(type == "SLAMS","SLAMS",ifelse(type == "TRIBAL","TRIBAL","SPM/OTHER")))),
  o3.monitors$monitor_type,o3.monitors$network)
t <- ddply(o3.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  CA=colors[3],NC=colors[2],SL=colors[1],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,CA=23,NC=22,SL=21,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,2,1,4,5)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/O3monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
  pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## SO2 monitor map
so2.monitors$site <- substr(so2.monitors$id,1,9)
so2.monitors$class <- mapply(function(type,network) ifelse(type == "INDUSTRIAL","INDUSTRIAL",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS",ifelse(type == "TRIBAL",
  "TRIBAL","SPM/OTHER")))),so2.monitors$monitor_type,so2.monitors$network)
t <- ddply(so2.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  IN=colors[3],NC=colors[2],SL=colors[1],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,IN=23,NC=22,SL=21,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,2,1,4,5)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/SO2monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
  pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## PM10 monitor map
pm10.monitors$site <- substr(pm10.monitors$id,1,9)
pm10.monitors$class <- mapply(function(type,network) ifelse(type == "INDUSTRIAL","INDUSTRIAL",
  ifelse(network %in% c("NCORE","NATTS","PROPOSED NCORE"),"NCORE/NATTS",ifelse(type == "SLAMS",
  "SLAMS",ifelse(type == "TRIBAL","TRIBAL","SPM/OTHER")))),
  pm10.monitors$monitor_type,pm10.monitors$network)
t <- ddply(pm10.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  IN=colors[3],NC=colors[2],SL=colors[1],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,IN=23,NC=22,SL=21,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,2,1,4,5)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM10monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
  pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## PM2.5 monitor map
pm25.monitors$site <- substr(pm25.monitors$id,1,9)
pm25.monitors$class <- mapply(function(type,network) ifelse(network == "NEAR ROAD","NEAR ROAD",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS",ifelse(type == "TRIBAL",
  "TRIBAL","SPM/OTHER")))),pm25.monitors$monitor_type,pm25.monitors$network)
t <- ddply(pm25.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  NE=colors[3],NC=colors[2],SL=colors[1],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,NE=23,NC=22,SL=21,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,1,2,4,5)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM25monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
  pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## Regulatory Pb monitor map
pb.monitors$site <- substr(pb.monitors$id,1,9)
t <- ddply(pb.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],type=min(monitor_type))
t$color <- sapply(tolower(t$type),switch,slams=colors[1],
  tribal=colors[2],industrial=colors[3],spm=colors[4],colors[4])
t$pch <- sapply(tolower(t$type),switch,slams=21,tribal=22,industrial=23,24)
N <- table(t$type)[c(2,1,3,4)]
legend.lab <- paste(c("SLAMS","INDUSTRIAL","SPM/OTHER","TRIBAL")," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/Pbmonitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors[c(1,3,4,2)],pch=c(21,23,24,22),
  cex=1.5,pt.cex=2,bty='n',ncol=4,title="Monitor Type (# Sites)")
dev.off()

## Non-regulatory Lead (Pb-PM10 and Pb-PM2.5) monitor map
pb.nonreg.monitors$site <- substr(pb.nonreg.monitors$id,1,9)
pb.nonreg.monitors$class <- sapply(pb.nonreg.monitors$network,function(x) ifelse(grepl("CSN",x),"CSN",
  ifelse(grepl("IMPROVE",x),"IMPROVE",ifelse(grepl("NCORE",x) | grepl("NATTS",x),"NATTS","OTHER"))))
t <- ddply(pb.nonreg.monitors,c("parameter","site"),summarize,
  latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,1),function(x) switch(x,
  C=colors[1],I=colors[2],N=colors[3],colors[4]))
t$pch <- sapply(t$parameter,function(x) ifelse(x == "88128",21,ifelse(x %in% c("82128","85128"),22,24)))
t <- t[order(t$class,t$pch,decreasing=TRUE),]
N1 <- table(t$pch); N2 <- table(t$class);
legend.lab1 <- paste(c("Pb-PM2.5","Pb-PM10","Pb-TSP STP")," (",N1,")",sep="")
legend.lab2 <- paste(names(N2)," (",N2,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/Pbnonregmonitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.2))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
## Draw NATTS sites back on top
natts <- subset(t,class == "NATTS" & parameter != "88128")
add.layer(type="points",x=natts$longitude,y=natts$latitude,
  proj.args=pa,bg="yellow3",pch=22,cex=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=legend.lab1,pch=c(21,22,24),pt.bg="white",
  cex=1.5,pt.cex=2,bty='n',ncol=3,title="Measurement Type (# Sites)")
legend("bottom",legend=legend.lab2,pt.bg=colors,pch=21,cex=1.5,
  pt.cex=2,bty='n',ncol=4,title="Monitoring Network (# Sites)")
dev.off()

## Coarse Particulates (PM10-2.5) monitor map
pm10_25.monitors$site <- substr(pm10_25.monitors$id,1,9)
pm10_25.monitors$class <- mapply(function(type,network) ifelse(network == "IMPROVE","IMPROVE",
  ifelse(grepl("NCORE",network),"NCORE",ifelse(type == "SLAMS","SLAMS",ifelse(type == "TRIBAL",
  "TRIBAL","SPM/OTHER")))),pm10_25.monitors$monitor_type,pm10_25.monitors$network)
t <- ddply(pm10_25.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  SL=colors[3],IM=colors[1],NC=colors[2],TR=colors[5],colors[4]))
t$pch <- sapply(substr(t$class,1,2),function(x) switch(x,IM=21,NC=22,SL=23,TR=25,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM10-25monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,pch=c(21:25),cex=1.5,
  pt.cex=2,bty='n',ncol=5,title="Monitoring Network (# Sites)")
dev.off()

## PM2.5 speciation monitor map
pm25.spec.monitors <- subset(pm25.spec.monitors,parameter == "88403")
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
pm25.spec.monitors$class <- sapply(pm25.spec.monitors$network,function(x)
  ifelse(grepl("CSN",x),"CSN",ifelse(grepl("IMPROVE",x),"IMPROVE","OTHER")))
t <- ddply(pm25.spec.monitors,"site",summarize,latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,1),function(x) switch(x,
  C=colors[1],I=colors[2],colors[4]))
t$pch <- sapply(substr(t$class,1,1),function(x) switch(x,C=21,I=22,24))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM25specmonitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
par(mar=c(0,0,0,0),bg="gray95")
draw.map("state",proj.args=pa,hires=TRUE,col="ivory2")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors[c(1,2,4)],pch=c(21,22,24),
  cex=1.5,pt.cex=2,bty='n',ncol=3,title="Monitoring Network (# Sites)")
dev.off()

cat("Done.",as.character(round(Sys.time())),"\n")