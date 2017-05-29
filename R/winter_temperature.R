winter_temp <- function(temp_dataset, xy_coordinates,
                        timeline_vec){
  
  # extract temperature
  freiburg_temp <- as.numeric(extract(temp_dataset, xy_coordinates))
  
  # build data frame
  temp_data_frame <- data.frame(date = timeline_vec,
                                month = month(timeline_vec),
                                year = year(timeline_vec),
                                temp = freiburg_temp)
  
  # only wubter month (december and january)
  temp_winter <- subset(temp_data_frame, 
                        month %in% c(12, 1))
  
  # give December/January the same year information
  temp_winter$newyear <- ifelse(temp_winter$month == 12, 
                                temp_winter$year + 1,
                                temp_winter$year)
  
  temp_winter
}