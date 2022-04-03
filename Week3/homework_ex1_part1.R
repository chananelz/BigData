#avishy alihi 
#Chananel Zaguri 206275711
A <- read.delim('Week3/table.tsv') # read the data from the file
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

B <- A[ order(A$DateTime), ] # order the data acording to the time


# create data frame:
BPAT_DF <- data.frame(time = B$DateTime, BPATNet = B$Net.generation )
CISO_DF <- data.frame(time = B$DateTime,CISONet = B$Net.generation.1 )
CPLE_DF <- data.frame(time = B$DateTime,CPLENet = B$Net.generation.2)
ERCO_DF <- data.frame(time = B$DateTime,ERCONet = B$Net.generation.3 )
FPL_DF <- data.frame(time = B$DateTime,FPLNet = B$Net.generation.4 )
ISNE_DF <- data.frame(time = B$DateTime,ISNENet = B$Net.generation.5 )
MISO_DF <- data.frame(time = B$DateTime,MISONet = B$Net.generation.6 )
NYIS_DF <- data.frame(time = B$DateTime,NYISNet = B$Net.generation.7 )
PACW_DF <- data.frame(time = B$DateTime,PACWNet = B$Net.generation.8 )
PJM_DF <- data.frame(time = B$DateTime,PJMNet = B$Net.generation.9 )


# convert the time to according the east cost
start <- as.POSIXct("2021-02-07 00:00:00",tz = "EST")
end <- as.POSIXct("2021-02-14 00:00:00",tz = "EST")

BPAT_DF <-BPAT_DF[ which(BPAT_DF$time >= start -3*60*60 & BPAT_DF$time < end -3*60*60), ]
CISO_DF <-CISO_DF[ which(CISO_DF$time >= start -3*60*60 & CISO_DF$time < end -3*60*60), ]
CPLE_DF <-CPLE_DF[ which(CPLE_DF$time >= start & CPLE_DF$time < end), ] 
ERCO_DF <-ERCO_DF[ which(ERCO_DF$time >= start -1*60*60 & ERCO_DF$time < end -1*60*60), ]
FPL_DF <- FPL_DF [ which(FPL_DF$time >= start & FPL_DF$time < end), ]
ISNE_DF <-ISNE_DF[ which(ISNE_DF$time >= start & ISNE_DF$time < end), ]
MISO_DF <-MISO_DF[ which(MISO_DF$time >= start -1*60*60 & MISO_DF$time < end -1*60*60), ]
NYIS_DF <-NYIS_DF[ which(NYIS_DF$time >= start & NYIS_DF$time < end), ]
PACW_DF <-PACW_DF[ which(PACW_DF$time >= start -3*60*60 & PACW_DF$time < end -3*60*60), ]
PJM_DF <- PJM_DF [ which(PJM_DF$time >= start & PJM_DF$time < end), ]

#creat df and clean the data
netCube = cbind(PJM_DF,CISO_DF$CISONet,CPLE_DF$CPLENet,ERCO_DF$ERCONet,FPL_DF$FPLNet,ISNE_DF$ISNENet,MISO_DF$MISONet,NYIS_DF$NYISNet,PACW_DF$PACWNet,BPAT_DF$BPATNet)
netCube <- unique(netCube)

head(netCube)





netCube$avrege_gen <- c(1) # initialization of the column and calc the sum
netCube$avrege_gen <- rowMeans(netCube[,c('PJMNet','CISO_DF$CISONet','CPLE_DF$CPLENet','ERCO_DF$ERCONet','FPL_DF$FPLNet' ,'ISNE_DF$ISNENet','MISO_DF$MISONet','NYIS_DF$NYISNet','PACW_DF$PACWNet' )],na.rm = TRUE,dims = 1)
netCube$myday <- format(netCube$time,"%d")
daily_power_generation <-tapply(netCube$avrege_gen, netCube$myday,FUN=sum ) # sum the data according to the day




# plot to pdf
pdf(file = "Week3/ex1_A.pdf",   # The directory I want to save the file in
    width = 5, # The width height of the plot in inches
    height = 4)
plot(daily_power_generation,xaxt = "n", type ='b', xlab="day of the week", ylab="network gen",main = "mean daily power generation across the US")
days <- c(7,8,9,10,11,12,13)
axis(1, at=1:7, labels=days)
dev.off()

