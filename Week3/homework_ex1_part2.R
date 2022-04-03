#avishy alihi 
#Chananel Zaguri 206275711
A <- read.delim('Week3/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

B <- A[ order(A$DateTime), ]


# step 1 create data cube from data frame:

BPAT_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "BPAT" ,damand  = B$Demand )
CISO_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "CISO" ,damand  = B$Demand.1 )
CPLE_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "CPLE" ,damand  = B$Demand.2)
ERCO_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "ERCO" ,damand  = B$Demand.3 )
FPL_DF <-  data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "FPL"  ,damand  = B$Demand.4 )
ISNE_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "ISNE" ,damand  = B$Demand.5 )
MISO_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "MISO" ,damand  = B$Demand.6 )
NYIS_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "NYIS" ,damand  = B$Demand.7 )
PACW_DF <- data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "PACW" ,damand  = B$Demand.8 )
PJM_DF <-  data.frame(year = format(B$DateTime,"%Y") ,month = format(B$DateTime,"%m"), day = format(B$DateTime,"%d") ,hour = format(B$DateTime,"%H") , location = "PJM"  ,damand  = B$Demand.9 )

#union the row from all the data frame
df_demand = rbind(BPAT_DF, CISO_DF, CPLE_DF, ERCO_DF, FPL_DF , ISNE_DF, MISO_DF, NYIS_DF, PACW_DF, PJM_DF )
df_demand <- na.omit(df_demand) 

# creat the data cube
demand_cube <- tapply(df_demand$damand ,
                      df_demand[,c("year","month", "day","hour" ,"location")],
                      FUN = mean )

print(demand_cube)
print (dimnames(demand_cube))

# step 2 dice the cube and create cube of location = PJM, NYIS, ISNE, FPL, CPLE and time = 10:00-18:00, 20:00-03:00 

dice_demand_cube <-
  demand_cube[ ,
               ,
               ,
               c("10" ,"11" ,"12" ,"13" ,"14" ,"15" ,"16" ,"17" ,"18"),
               c("PJM", "NYIS", "ISNE", "FPL", "CPLE")]

print(dice_demand_cube)
print (dimnames(dice_demand_cube))

# step 3 roll up the cube and create data frame of demand per hour:

roll_up_demand_cube <- 
  apply(dice_demand_cube, c("hour"),
        FUN = function(x) mean(x, na.rm = TRUE) )

demand_matrix = as.data.frame(roll_up_demand_cube)
demand_matrix <- cbind(hour = rownames(demand_matrix), demand_matrix)
rownames(demand_matrix) <- 1:nrow(demand_matrix)

demand_vector = demand_matrix$roll_up_demand_cube
demand_vector <- as.matrix(demand_vector)

# step 4 calculate the linear regression according to the slides

demand_matrix$hour <- c("0","1","2","3","4","5","6","7","8")


U = data.frame(free = c(1) , hour = demand_matrix$hour)
U <- as.matrix(sapply(U, as.numeric))
U_tran = t(U)
inverse_UU = solve(U_tran %*% U )
UUU = inverse_UU %*% U_tran
result <- UUU %*% demand_vector



pdf(file = "Week3/ex2_A.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4)
plot(demand_matrix$hour, demand_matrix$roll_up_demand_cube, pch=19,xaxt = "n", xlab="hour", ylab="deman",main = "minute power demand in the east coast: 10:00-18:00")
minits <- c(10 ,11 ,12 ,13 ,14 ,15 ,16 ,17 ,18)
axis(1, at=0:8, labels=minits)
abline(result[1], result[2])

dev.off()



# ------------------------------for 20:00 - 03:00 ---------------------------------

dice_demand_cube <-
  demand_cube[ ,
               ,
               ,
               c("20","21","22","23","00","01","02","03"),
               c("PJM", "NYIS", "ISNE", "FPL", "CPLE")]

print(dice_demand_cube)
print (dimnames(dice_demand_cube))

# step 3 roll up the cube and create data frame of demand per hour:

roll_up_demand_cube <- 
  apply(dice_demand_cube, c("hour"),
        FUN = function(x) mean(x, na.rm = TRUE) )

demand_matrix = as.data.frame(roll_up_demand_cube)
demand_matrix <- cbind(hour = rownames(demand_matrix), demand_matrix)
rownames(demand_matrix) <- 1:nrow(demand_matrix)

demand_vector = demand_matrix$roll_up_demand_cube
demand_vector <- as.matrix(demand_vector)

# step 4 calculate the linear regression according to the slides

demand_matrix$hour <- c("0","1","2","3","4","5","6","7")

U = data.frame(free = c(1) , hour = demand_matrix$hour)
U <- as.matrix(sapply(U, as.numeric))
U_tran = t(U)
inverse_UU = solve(U_tran %*% U )
UUU = inverse_UU %*% U_tran
result <- UUU %*% demand_vector



pdf(file = "Week3/ex2_B.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4)
plot(demand_matrix, xaxt = "n", xlab="hour", ylab="deman",main = "minute power demand in the east coast: 10:00-18:00")

minits <- c(20,21,22,23,00,01,02,03)
axis(1, at=0:7, labels=minits)
abline(result[1], result[2])
dev.off()











