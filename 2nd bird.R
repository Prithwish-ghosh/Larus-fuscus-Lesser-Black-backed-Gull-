library(readr)
library(circular)
options(rgl.useNULL = TRUE)
library(Directional)
setwd("~/Downloads")
dataset = read.csv(file.choose())
head(dataset)
Mig_bird = data.frame(dataset)
head(Mig_bird)
sum(is.na(Mig_bird$location.long))
sum(is.na(Mig_bird$location.lat))
watson.test(Mig_bird$location.long , alpha = 0.01 , dist = "vonmises")
watson.test(Mig_bird$location.lat, alpha = 0.01, dist = "vonmises")
dim(Mig_bird)

library(maps)
world_coordinates = map_data("world")
northern_map <- subset(world_coordinates, long >0)
northern_map1 <- subset(northern_map , long <60)
northern_map2 <- subset(northern_map1 , lat >= -15)
northern_map3 <- subset(northern_map2 , lat <= 75)
ggplot() +
  geom_map(
    data = northern_map3,map = northern_map3,
    aes(long, lat , map_id = region),
    color = "black" , fill = "lightblue"
  ) +
  geom_point(
    data = Mig_bird,
    aes(Mig_bird$location.long , Mig_bird$location.lat , color = "red",
    ) ,
    alpha = 1, size = 0.5
  ) +
  theme(legend.position = "top")

#initial Bearing of the birdpath

get_initial_bearing <- function(longA, latA, longB, latB) {
  delta_long <- longB - longA
  rad_latA <- latA * (pi / 180)  # Convert latA to radians
  rad_latB <- latB * (pi / 180)  # Convert latB to radians
  
  bearing <- atan2(sin(delta_long) * cos(rad_latB), cos(rad_latA) * sin(rad_latB) - sin(rad_latA) * cos(rad_latB) * cos(delta_long))
  bearing_degrees <- bearing * (180 / pi)  # Convert bearing to degrees
  
  return(bearing_degrees)
}
dim(Mig_bird)

for ( i in 1:89867) {
  #Black_crowned_night_heron$s[i] = Black_crowned_night_heron$G_h[i+1] - Black_crowned_night_heron$G_h[i]
  Mig_bird$theta[i] = get_initial_bearing(Mig_bird$location.long[i] ,
                                          Mig_bird$location.lat[i],
                                          Mig_bird$location.long[i+1],
                                          Mig_bird$location.lat[i+1] )
}

#watson.test(Mig_bird$theta , alpha = 0.01 , dist = "vonmises")
tail(Mig_bird)

library(solarPos)
library(suncalc)
  
timestamp <- Mig_bird$timestamp

# Parse the timestamp into a datetime object
datetime <- strptime(timestamp, format = "%Y-%m-%d %H:%M:%S")

# Extract the date and time components
Mig_bird$date <- format(datetime, format = "%Y-%m-%d")
Mig_bird$time <- format(datetime, format = "%H:%M:%S")



Mig_B = Mig_bird[,c(4,5,16,17,18)]
head(Mig_B)
library(lubridate)

# Sample timestamps
timestamps <- Mig_B$date

# Convert the timestamps to Date objects
dates <- ymd(timestamps)  # ymd() function parses "YYYY-MM-DD" format

# Extract year, month, and day components
Mig_B$years <- year(dates)
Mig_B$months <- month(dates)
Mig_B$days <- day(dates)

tail(Mig_B)


timestamp <- Mig_B$time

# Parse the timestamp into a datetime object
datetime <- hms(timestamp)

# Extract the hours, minutes, and seconds
Mig_B$hour <- hour(datetime)
Mig_B$minute <- minute(datetime)
Mig_B$second <- second(datetime)

head(Mig_B)
Mig_B$JD = solarPos::julianDay(Mig_B$years , Mig_B$months, Mig_B$days , Mig_B$hour ,
                               Mig_B$minute, Mig_B$second)
head(Mig_B)
tail(Mig_B)

z = solarPos::solarPosition(Mig_B$JD , Mig_B$location.long , Mig_B$location.lat)
head(z)
Mig_B$zenith =  z[,1]
Mig_B$azimuth = z[,2]

data = data.frame(date = Mig_B$date, lat = Mig_B$location.lat , lon = Mig_B$location.long)

z = suncalc::getMoonPosition(data = data,  keep = c("altitude", "azimuth"))
z

Mig_B$Moon_azimuth = z$azimuth

head(Mig_B)
watson.test(Mig_B$Moon_azimuth, alpha = 0.05, dist = "vonmises")

head(Mig_B)

# Convert the date column to a Date object
Mig_B$date <- as.Date(Mig_B$date)

# Split the data frame by month
result <- split(Mig_B, format(Mig_B$date, "%Y-%m"))

summary(result[[76]])

d1 = rbind(result[[1]], result[[2]], result[[3]], result[[4]])
d2 = rbind(result[[5]], result[[6]])
d22= rbind(result[[7]], result[[8]])
dim(d2)
dim(d22)
d3 = rbind(result[[9]], result[[10]], result[[11]], result[[12]])
d4 = rbind(result[[13]], result[[14]], result[[15]], result[[16]])
d5 = rbind(result[[17]], result[[18]], result[[19]], result[[20]])
d6 = rbind(result[[21]], result[[22]], result[[23]], result[[24]])
d7 = rbind(result[[25]], result[[26]], result[[27]], result[[28]])
d8 = rbind(result[[29]], result[[30]], result[[31]], result[[32]])
d9 = rbind(result[[33]], result[[34]], result[[35]], result[[36]])
d10 = rbind(result[[37]], result[[38]], result[[39]], result[[40]])
d11 = rbind(result[[41]], result[[42]], result[[43]], result[[44]])
d12 = rbind(result[[45]], result[[46]], result[[47]], result[[48]])
d13 = rbind(result[[49]], result[[50]], result[[51]], result[[52]])
d14 = rbind(result[[53]], result[[54]], result[[55]], result[[56]])
d15 = rbind(result[[57]], result[[58]], result[[59]], result[[60]])
d16 = rbind(result[[61]], result[[62]], result[[63]], result[[64]])
d17 = rbind(result[[65]], result[[66]], result[[67]], result[[68]])
d18 = rbind(result[[69]], result[[70]], result[[71]], result[[72]])
d19 = rbind(result[[73]], result[[74]], result[[75]], result[[76]])


tail(Mig_B)
summary(Mig_B)
dim(d3)
fit1 = lm.circular(d1$theta, d1$zenith)
fit11 = lm.circular(d1$theta, d1$azimuth)
f1 =lm.circular(d1$theta , d1$Moon_azimuth)

fit2 = lm.circular(d2$theta, d2$zenith)
fit22 = lm.circular(d2$theta, d2$azimuth)
f2 = lm.circular(d2$theta, d2$Moon_azimuth)

fit12 = lm.circular(d22$theta, d22$azimuth)
fit122 = lm.circular(d22$theta, d22$zenith)
f12 = lm.circular(d22$theta, d22$Moon_azimuth)

fit3 = lm.circular(d3$theta, d3$zenith)
fit33 = lm.circular(d3$theta, d3$azimuth)
f3= lm.circular(d3$theta, d3$Moon_azimuth)

fit4 = lm.circular(d4$theta, d4$zenith)
fit44 = lm.circular(d4$theta, d4$azimuth)
f4 = lm.circular(d4$theta, d4$Moon_azimuth)

fit5 = lm.circular(d5$theta, d5$zenith)
fit55 = lm.circular(d5$theta, d5$azimuth)
f5 = lm.circular(d5$theta, d5$Moon_azimuth)

fit6 = lm.circular(d6$theta, d6$zenith)
fit66 = lm.circular(d6$theta, d6$azimuth)
f6 = lm.circular(d6$theta, d6$Moon_azimuth)

fit7 = lm.circular(d7$theta, d7$zenith)
fit77 = lm.circular(d7$theta, d7$azimuth)
f7 = lm.circular(d7$theta, d7$Moon_azimuth)

fit8 = lm.circular(d8$theta, d8$zenith)
fit88 = lm.circular(d8$theta, d8$azimuth)
f8 = lm.circular(d8$theta, d8$Moon_azimuth)

fit9 = lm.circular(d9$theta, d9$zenith)
fit99 = lm.circular(d9$theta, d9$azimuth)
f9 = lm.circular(d9$theta, d9$Moon_azimuth)

fit10 = lm.circular(d10$theta, d10$zenith)
fit110 = lm.circular(d10$theta, d10$azimuth)
f10 = lm.circular(d10$theta, d10$Moon_azimuth)

fit11 = lm.circular(d11$theta, d11$zenith)
fit111 = lm.circular(d11$theta, d11$azimuth)
f11 = lm.circular(d11$theta , d11$Moon_azimuth)

fit21 = lm.circular(d12$theta, d12$zenith)
fit221 = lm.circular(d12$theta, d12$azimuth)
f21 = lm.circular(d12$theta, d12$Moon_azimuth)

fit31 = lm.circular(d13$theta, d13$zenith)
fit331 = lm.circular(d13$theta, d13$azimuth)
f31 = lm.circular(d13$theta, d13$Moon_azimuth)

fit41 = lm.circular(d14$theta, d14$zenith)
fit441 = lm.circular(d14$theta, d14$azimuth)
f41 = lm.circular(d14$theta, d14$Moon_azimuth)

fit51 = lm.circular(d15$theta, d15$zenith)
fit551 = lm.circular(d15$theta, d15$azimuth)
f51 = lm.circular(d15$theta , d15$Moon_azimuth)

fit61 = lm.circular(d16$theta, d16$zenith)
fit661 = lm.circular(d16$theta, d16$azimuth)
f61 = lm.circular(d16$theta , d16$Moon_azimuth)

fit71 = lm.circular(d17$theta, d17$zenith)
fit771 = lm.circular(d17$theta, d17$azimuth)
f71 = lm.circular(d17$theta, d17$Moon_azimuth)

fit81 = lm.circular(d18$theta, d18$zenith)
fit881 = lm.circular(d18$theta, d18$azimuth)
f81 = lm.circular(d18$theta , d18$Moon_azimuth)

fit91 = lm.circular(d19$theta, d19$zenith)
fit991 = lm.circular(d19$theta, d19$azimuth)
f91 = lm.circular(d19$theta , d19$Moon_azimuth)

library(CircStats)
library(Directional)

library(movMF)

d = cbind(Mig_B$location.lat, Mig_B$location.long)
Evmf <- function(K){
  movMF(d, k= K, control = list(nruns = 20))
}

Esd = lapply(1:20, Evmf)
Esd
sapply(Esd, BIC)

# nOW WE USE 16000 SAMPLES TO PLOT

vmf_density_grid = function(u, ngrid = 100){
  u[,1] <- u[,1] + 90
  u[,2] <- u[,2] +180
  res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                         ngrid = ngrid)
  ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
  ret$Density <- c(res$den)
  ret
}


sample_size = 15000
sampled_data <- Mig_B[sample(nrow(Mig_B), sample_size, replace = F),]

sb = cbind(sampled_data$location.lat, sampled_data$location.long)

b.dens = vmf_density_grid(sb, ngrid = 300)

watson.test(Mig_B$theta, alpha = 0.01 , dist = "vonmises")
watson.test(Mig_B$zenith, alpha = 0.01 , dist = "vonmises")
watson.test(Mig_B$azimuth, alpha = 0.01 , dist = "vonmises")
watson.test(Mig_B$Moon_azimuth, alpha = 0.01, dist = "vonmises")

library(ggplot2)
library(maps)


world_coordinates = map_data("world")
northern_map <- subset(world_coordinates, long >0)
northern_map1 <- subset(northern_map , long <60)
northern_map2 <- subset(northern_map1 , lat >= -15)
northern_map3 <- subset(northern_map2 , lat <= 75)


 ggplot()+
  geom_map(data = northern_map3, map = northern_map3,
           aes(long, lat,map_id = region),
           color = "black", fill = "white") +
  geom_point(data = Mig_B,
             mapping = aes(x = Mig_B$location.long, y = Mig_B$location.lat),
             color = "red", alpha = 0.5, size = 1 , stroke = 0.1) +
  geom_density_2d(data = Mig_B,
                  aes(x = Mig_B$location.long, y = Mig_B$location.lat),
                  color = "green", alpha = 2) +
  geom_contour(data = b.dens, aes(x = Long, y = Lat, z = Density),
               color = "blue") #+
#  coord_map("mercator")

 ggplot()+
   geom_map(data = northern_map3, map = northern_map3,
            aes(long, lat,map_id = region),
            color = "black", fill = "white") +
   geom_point(data = Mig_B,
              mapping = aes(x = Mig_B$location.long, y = Mig_B$location.lat),
              color = "red", alpha = 0.5, size = 1 , stroke = 0.1) +
   geom_density_2d(data = Mig_B,
                   aes(x = Mig_B$location.long, y = Mig_B$location.lat),
                   color = "green", alpha = 2) +
   geom_contour(data = b.dens, aes(x = Long, y = Lat, z = Density),
                color = "blue")+
   coord_map("orthographic", orientation = c(0, 30, 0)) +
   scale_x_continuous(breaks = seq(-180, 180, 20)) +
   scale_y_continuous(breaks = seq(-90, 90, 45)) +
   ggtitle("Orthographic Projection of Spherical Density", "Top / Front View") +
   xlab("") +
   ylab("") +
   theme(axis.ticks = element_blank(),
         axis.text = element_blank(),
         panel.ontop = TRUE,
         legend.position = "none",
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         panel.grid = element_line(color = "black" ),
         panel.background = element_rect(fill = NA))



library(Directional)

u = euclid(d)
fishkent(u)

b.dens

file = "density.csv"
write.csv(b.dens, file = file, row.names = F)
file = "Mig_B.csv"
write.csv(Mig_B, file = file, row.names = F)
