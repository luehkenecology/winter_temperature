timeline <- function(start_year = 2016,end_year = 2016,
                     start_month = 1, end_month = 12,
                     start_day_of_month = 1, 
                     end_day_of_month = 31){
  
  A1<-paste(start_year, "-", start_month, "-", start_day_of_month, sep = "")
  A2<-paste(end_year, "-", end_month, "-", end_day_of_month, sep = "")
  time.s=as.POSIXct(A1,tz='UTC')
  time.e = as.POSIXct(A2,tz='UTC')
  seq(time.s, time.e, by='24 hours')
}