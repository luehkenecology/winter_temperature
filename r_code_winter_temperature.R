# clear memory
rm(list = ls())

#load required packages 
library(raster)
library(lubridate)
library(plyr)
library(RcppRoll)
library(rworldmap)

# working directory
setwd("G:/NeuAll/Research_projects/winter_temperature")


# years of interest
start_year_sub <- 1950
end_year_sub  <- 2016

# load temperature data
temp_sub <- lapply(start_year_sub:end_year_sub, function(i) stack(paste("G:/NeuAll/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v14.0_europe_", 
                                                                        i, ".grd", sep = "")))
temp_sub <- lapply(start_year_sub:end_year_sub, function(i) stack(paste("C:/Users/RenkeLuehken/Google Drive/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v14.0_europe_", 
                                                                        i, ".grd", sep = "")))

temp_sub_2 <- stack(unlist(temp_sub))  

ger_shape_r <- raster::getData('GADM', country="DEU", level=1)

# crop raster
dfdfLLL <- crop(temp_sub_2, ger_shape_r)
#dfdfLLL <- crop(temp_sub_2, c(-11, 41, 36, 60))

#---------- raster to matrix
gdg2 <- getValues(dfdfLLL)

#---------- loop through cells

# parameter


ee <- timeline(start_year_sub,end_year_sub)

# winter month Freiburg
freiburg_temp <- as.numeric(extract(dfdfLLL, cbind(7.843,
                                                   48.016))) 

freiburg_temp_dat <- data.frame(dates = ee,
                                month = month(ee),
                                year = year(ee),
                                temp = freiburg_temp)
freiburg_temp_winter <- subset(freiburg_temp_dat, 
                               month %in% c(12, 1))
freiburg_temp_winter$newyear <- ifelse(freiburg_temp_winter$month == 12, 
                                       freiburg_temp_winter$year + 1,
                                       freiburg_temp_winter$year)
freiburg_temp_winter_sub <- subset(freiburg_temp_winter, month %in% c(12, 1))

AA_frei <- vector()
for(i in 10:1){
  AA_frei[i] <- mean(subset(freiburg_temp_winter_sub, year > (2017-i-29) & year < 2017-i)$temp)
}

# Heidelberg
heidelberg_temp <- as.numeric(extract(dfdfLLL, cbind(8.652,
                                                     49.411))) 
heidelberg_temp_dat <- data.frame(dates = ee,
                                  month = month(ee),
                                  year = year(ee),
                                  temp = heidelberg_temp)
heidelberg_temp_winter <- subset(heidelberg_temp_dat, month %in% c(12, 1))
heidelberg_temp_winter$newyear <- ifelse(heidelberg_temp_winter$month == 12, 
                                         heidelberg_temp_winter$year + 1,
                                         heidelberg_temp_winter$year)
heidelberg_temp_winter_sub <- subset(heidelberg_temp_winter, month %in% c(12, 1))

AA_heid <- vector()
for(i in 10:1){
  AA_heid[i] <- mean(subset(heidelberg_temp_winter_sub, year > (2017-i-29) & year < 2017-i)$temp)
}

# mean temp
mean.winter.temp <- data.frame(parameter = "mean winter temp", rbind(cbind(city = "Freiburg", ddply(freiburg_temp_winter_sub, .(newyear), 
                                                                                                    summarize, mean = mean(temp))),
                                                                     cbind(city = "Heidelberg", ddply(heidelberg_temp_winter_sub, .(newyear), 
                                                                                                      summarize, mean = mean(temp)))))

# n day below zero
n.day.below.zero <- data.frame(parameter = "n day below zero", rbind(cbind(city = "Freiburg", ddply(freiburg_temp_winter_sub, .(newyear), 
                                                                                                    summarize, mean = sum(temp<0))),
                                                                     cbind(city = "Heidelberg", ddply(heidelberg_temp_winter_sub, .(newyear), 
                                                                                                      summarize, mean = sum(temp<0)))))
temp.both <- rbind(mean.winter.temp, n.day.below.zero)
temp.both.sub <- subset(temp.both, 
                        newyear > 2006 & newyear < 2017)

temp.both.sub2 <- data.frame(parameter = "deviation",      city = c(rep("Freiburg", 10),
                                                                    rep("Heidelberg", 10)) ,
                             newyear  =c(seq(2007, 2016, 1),
                                         seq(2007, 2016, 1)) ,
                             mean = temp.both.sub$mean[1:20]-c(AA_frei, AA_heid))


library(ggplot2)
png(file = "ij.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(temp.both.sub, parameter == "mean winter temp"), 
       aes(newyear, mean)) + 
  geom_line() + 
  xlab("year") +
  ylab("mean temperature (Dec/Jan)") +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "ij2.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(temp.both.sub, parameter == "n day below zero"), 
       aes(newyear, mean)) + 
  geom_line() + 
  xlab("year") +
  ylab("number of days below zero (Dec/Jan)") +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "ij_full.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(temp.both, parameter == "mean winter temp"), 
       aes(newyear, mean)) + 
  geom_line() + 
  xlab("year") +
  ylab("mean temperature (Dec/Jan)") +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "ij2_full.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(temp.both, parameter == "n day below zero"), 
       aes(newyear, mean)) + 
  geom_line() + 
  xlab("year") +
  ylab("number of days below zero (Dec/Jan)") +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "ij_3.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(temp.both.sub2, 
       aes(newyear, mean)) + 
  geom_bar(stat = "identity") + 
  xlab("year") +
  ylab(" deviation from  30-year mean  (Dec/Jan)") +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()