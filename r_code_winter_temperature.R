# clear memory
rm(list = ls())

# load required packages 
library(raster)
library(lubridate)
library(plyr)
library(ggplot2)

# working directory
setwd("G:/NeuAll/Research_projects/winter_temperature")

# function
source("R/timeline.R")
source("R/winter_temperature.R")

# years of interest
start_year_sub <- 1950
end_year_sub  <- 2016

# load temperature data
temp_sub <- lapply(start_year_sub:end_year_sub, function(i) stack(paste("G:/NeuAll/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v14.0_europe_", 
                                                                        i, ".grd", sep = "")))


# stack the rasters for each year
temp_sub_2 <- stack(unlist(temp_sub))  

# read shape of Germany
ger_shape_r <- getData('GADM', country="DEU", level=1)

# crop raster by shape
temp_crop <- crop(temp_sub_2, ger_shape_r)

# transform raster to matrix
gdg2 <- getValues(temp_crop)

# loop through cells
timesteps <- timeline(start_year_sub,
               end_year_sub)

# winter month Freiburg
freiburg_coordinates <- cbind(7.843,
                              48.016)
heidelberg_coordinates <- cbind(8.652,
                              49.411)

# extract data
freiburg_res <- winter_temp(temp_dataset = temp_crop,
                  xy = freiburg_coordinates,
                  timeline_vec = timesteps)
heidelberg_res <- winter_temp(temp_dataset = temp_crop,
                             xy = heidelberg_coordinates,
                             timeline_vec = timesteps)

# min temp data file
min.winter.temp <- data.frame(parameter = "min winter temp", rbind(cbind(city = "Freiburg", ddply(freiburg_res, .(newyear), 
                                                                                                    summarize, mean =  sum(temp[temp<0]))),
                                                                     cbind(city = "Heidelberg", ddply(heidelberg_res, .(newyear), 
                                                                                                      summarize, mean = sum(temp[temp<0])))))

# mean temp data file
mean.winter.temp <- data.frame(parameter = "mean winter temp", rbind(cbind(city = "Freiburg", ddply(freiburg_res, .(newyear), 
                                                                                                    summarize, mean = mean(temp))),
                                                                     cbind(city = "Heidelberg", ddply(heidelberg_res, .(newyear), 
                                                                                                      summarize, mean = mean(temp)))))

# n day below zero data file
n.day.below.zero <- data.frame(parameter = "n day below zero", rbind(cbind(city = "Freiburg", ddply(freiburg_res, .(newyear), 
                                                                                                    summarize, mean = sum(temp<0))),
                                                                     cbind(city = "Heidelberg", ddply(heidelberg_res, .(newyear), 
                                                                                                      summarize, mean = sum(temp<0)))))

# merge mean temp data file & n day below zero data file
temp.both <- rbind(min.winter.temp, mean.winter.temp, n.day.below.zero)

# subset last 10 years
temp.both.sub <- subset(temp.both, 
                        newyear > 2006 & newyear < 2017)

png(file = "figs/min_winter_temp_subset.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(temp.both.sub, parameter == "min winter temp"), 
       aes(newyear, mean)) + 
  geom_line() + 
  xlab("year") +
  ylab("sum of negative temperatures (Dec/Jan)") +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "figs/min_winter_temp_all.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(temp.both, parameter == "min winter temp"), 
       aes(newyear, mean)) + 
  geom_line() + 
  xlab("year") +
  ylab("sum of negative temperatures (Dec/Jan)") +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "figs/mean_winter_temp_subset.png",
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

png(file = "figs/number_of_days_below_zero_sub.png",
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

png(file = "figs/mean_winter_temp_all.png",
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

png(file = "figs/number_of_days_below_zero_all.png",
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


# deviation from  30-year mean  (Dec/Jan)

# calculate mean winter temperature over the last 30 years for Freiburg

AA_frei <- vector()
for(i in 37:1){
  AA_frei[i] <- mean(subset(freiburg_res, year > (2017-i-29) & year < 2017-i)$temp)
}

# calculate mean winter temperature over the last 30 years for Heidelberg
AA_heid <- vector()
for(i in 37:1){
  AA_heid[i] <- mean(subset(heidelberg_res, year > (2017-i-29) & year < 2017-i)$temp)
}

# merge data
temp.both.sub2 <- data.frame(city = c(rep("Freiburg", 37),
                                                                    rep("Heidelberg", 37)) ,
                             newyear  =c(seq(1980, 2016, 1),
                                         seq(1980, 2016, 1)) ,
                             mean = c(AA_frei, AA_heid))

OO <- merge(mean.winter.temp, temp.both.sub2, by = c("city", "newyear"))
OO$deviation <- OO$mean.x - OO$mean.y

png(file = "figs/deviation_from_30_year_mean_sub.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(subset(OO, newyear > 2006), 
       aes(newyear, deviation)) + 
  geom_bar(stat = "identity") + 
  xlab("year") +
  ylab("deviation from  30-year mean  (Dec/Jan)") +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()

png(file = "figs/deviation_from_30_year_mean_all.png",
    width = 6, height = 3, units = 'in', res = 500)
ggplot(OO, 
       aes(newyear, deviation)) + 
  geom_bar(stat = "identity") + 
  xlab("year") +
  geom_smooth(method = "lm", se = T) +
  ylab("deviation from  30-year mean  (Dec/Jan)") +
  facet_wrap(~ city , scales = "free_y") +
  theme_bw()
dev.off()