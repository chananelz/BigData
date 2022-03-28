A <- read.delim('Week3/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

B <- A[ order(A$DateTime), ]

mytabele = 



# analyze a time frame out of the power consumption data
rng <- 816:983
C <- with(B, cbind( Net.generation,Net.generation.1, Net.generation.2, Net.generation.3, Net.generation.4,Net.generation.5, 
                    Net.generation.6, Net.generation.7 , Net.generation.8 , Net.generation.9 , Net.generation.10  ))
C <- C[rng, ]


# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(0,12), ylim=c(-2, 2))

DF <- data.frame(Time = rng - min(rng), Total.Net.Generation = NA)
for( i in seq(168)){
  DF[i,2] <- mean(C[ i,]) 
}

DF2 <- data.frame(Time = 1:7, Total.Net.Generation = NA)
count <- 0
for (i in seq(7)){
  DF2[i,2] <- mean(DF[count:count+24,2])
  count <- count +24  
}

pdf(file = "Week3/ex1_A.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)
plot(DF2 , type ='b', main = "mean daily power generation across the US")
dev.off()

