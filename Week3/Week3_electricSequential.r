A <- read.delim('Week3/table.tsv') # read file difult is by tab 
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )#A  יצירת אובייקט זמן בתוך 
summary(A)

print(names(A)) # הדפסת שמות העמודות של הדאטא פריים שלי

B <- A[ order(A$DateTime), ] #  לפי זמנים  A  לסדר את השורות של 

print( as.integer(B[1, 'DateTime']) ) # הפיכת זמן למספר
print( B[1, 'DateTime'] < B[5, 'DateTime'] )
print( c( B[1, 'DateTime'] , B[5, 'DateTime'] ) ) #יוצרת רשימה מאוסף פרמטרים C פונקצייה 
print (
  which ( B[, 'DateTime'] < as.POSIXct("2021-02-14 23:59:99 EST") &
	  B[, 'DateTime'] > as.POSIXct("2021-02-07 00:00:00 EST" ) )
  )


# analyze a time frame out of the power consumption data
rng <- 816:1007 # תיחום זמנים לפי מספרי שורות

# print the range of times in our slice
print (B[rng, 'DateTime' ])

C <- with(B, cbind( Net.generation.1, Net.generation.2, Net.generation.3, Net.generation.4,Net.generation.5, 
                    Net.generation.6, Net.generation.7 , Net.generation.8 , Net.generation.9 , Net.generation.10  )) # יצירת דאטא פריים חדש תוך סכימת העמודות הללו בלבד 
# אומר לפתוח את בי ולבצע פעולות  with ה 

C <- C[rng, ] # רק את השורות בטווח הנתון C לקחת מיתוך 
print (C)


# mean values
M <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA, 
           Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )
# standard deviations
S <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA,
           Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )
# linear fit
LM <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA,
            Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )

# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(0,191), ylim=c(-2, 2))


DF <- data.frame(Time = rng - min(rng), Total.Net.Generation = NA)
for( i in seq(191)){
  DF[i,2] <- mean(C[ i,]) 
}

DF2 <- data.frame(Time = 1:7, Total.Net.Generation = NA)
count <- 0
for (i in seq(7)){
  DF2[i,2] <- mean(DF[count:count+24,2])
  count <- count +24  
}

norm.DF2 <- t( (t(DF2[,2]) - unlist(mean(DF2[,2]))) / unlist(sd(DF2[,2])) )
print(norm.DF2)

#----------------------------------------#


# calculate means and stdev
demands <- seq(0,7) # [ -c(1,4,8) ]
for ( i in demands ) {
  M[[ i ]] <- mean( C[ !is.na(C[, i]) ,i ] )
  S[[ i ]] <- sd( C[ !is.na(C[, i]), i ] )
}

# scale and center the consumption series (normalize)
norm.C <- t( (t(C) - unlist(M)) / unlist(S) )
print(norm.C)

demands <- seq(10)#[ -c(1,4,5,7,10) ]
for ( i in demands ) {
   # rearrange in a new, temporary dataframe
   DF <- data.frame ( Time = rng - min(rng), Demand = norm.C[ , i ] )
   # plot
   lines( DF, col = i, type = 'b' )
   # linear fit
   LM[[ i ]] <- lm( Demand ~ Time, data = DF)
   a <- coef(LM[[ i ]])[1]
   b <- coef(LM[[ i ]])[2]
   abline(a, b, col = i, lw =2)
}
legend( 'bottomleft', col = demands, pch = 19,
       legend = sapply(demands, function(x) paste0('Demand.',x) ))

# mean regression line
lm.df <- sapply(LM[ -c(1,4,5,7,10) ], function(c) coef(c))
n <- dim(lm.df)[2]
tot <- rowSums(lm.df) 
a <- tot[1] / n
b <- tot[2] / n

# plot the mean regression line
abline(a, b, col = 'black', lw = 2, lt = 2)
time.min <- format(B[rng, 'DateTime' ][1], "%H" )
time.max <- format(B[rng, 'DateTime' ][length(rng)], "%H" )
title(paste0("Normalized demand over " , time.min, '-', time.max) )
