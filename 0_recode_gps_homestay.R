### David Pagliaccio, updated Paul Bloom & Julia Greenblatt
### Recalculate daily homestay metrics using multiple home locations 

## load libraries ----
library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)
library(mapview)
theme_set(theme_bw(base_size = 12,base_family = "Arial")); theme_update(panel.grid.minor = element_line(linetype = "dashed",size=.5),axis.title.x = element_text(face = "bold"),axis.title.y = element_text(face = "bold"))

# Esha's function for converting epoch to local date/time
epoch2timestamplocal <- function(epoch, tz, milliseconds=TRUE){
  if (milliseconds) {dividend <- 1000} else{dividend <- 1}
  tz_sign = stringr::str_extract(tz, '[+-]')
  tz_hour = stringr::str_extract(tz, '[0-9]{1,2}(?=:)')
  tz_minute = stringr::str_extract(tz, '(?<=:)[0-9]{2}')
  tz_adjust = (as.numeric(tz_hour)*60*60 + as.numeric(tz_minute)*60)*dividend
  epoch_local = as.numeric(if_else(tz_sign == '+', epoch + tz_adjust, epoch - tz_adjust))
  options(digits.secs=0)
  return(as.POSIXct(as.numeric(epoch_local) / dividend, origin = '1970-01-01', tz = 'GMT')) # Removed as.integer to preserve ms
}


# function get the location where the participant spends the most total time between 2-6am based on the EARS trace
get_max_sleep_location = function(gps_trace){
  gps_trace <- gps_trace %>% mutate(local_time = epoch2timestamplocal(epoch=epoch, tz=timezone),
                                hour = hour(local_time)) %>% 
    dplyr::filter(2<=hour & hour<=6) %>% 
    group_by(val_lat, val_lon) %>% 
    summarise(total_time = sum(amt_gps_duration_sec))
  
  max_sleep_time_coordinates = gps_trace %>%
      ungroup() %>%
      dplyr::arrange(-total_time) %>%
      mutate(rank = 1:n()) %>%
      dplyr::filter(rank ==1)
  
  
  return(max_sleep_time_coordinates)
  
}

# Pull in mailing address coordinates for CU participants
cu_mailing_coordinates = openxlsx::read.xlsx('mailing_address_lat_long.xlsx')
cu_mailing_coordinates = mutate(cu_mailing_coordinates, 
                             val_lat = as.numeric(val_lat),
                             val_long = as.numeric(val_long),
                             ID = as.character(ID))

cu_mailing_coordinates = dplyr::filter(cu_mailing_coordinates, !is.na(val_lat), !is.na(val_long))
cu_mailing_coordinates = dplyr::select(cu_mailing_coordinates, ID, val_lat, val_long)

# Pull in mailing address coordinates for Pitt Participants
pitt_mailing_coordinates = openxlsx::read.xlsx('qry_Latitude_Longitude.xlsx') %>%
  dplyr::filter(!duplicated(PTID)) %>%
  mutate(ID = as.character(PTID),
         val_lat = as.numeric(LATITUDE),
         val_long = as.numeric(LONGITUDE)) %>%
  dplyr::select(ID, val_lat, val_long)

mailing_coordinates = rbind(cu_mailing_coordinates, pitt_mailing_coordinates)
mailing_coordinates = dplyr::filter(mailing_coordinates, !is.na(val_lat), !is.na(val_long))

# Lits of participant IDs
id_list = dir('/Volumes/columbia/MAPS_Data/EARS_Features/GPS/')

# Function to create a recoded GPS (homestay) file with multiple home locations on a daily level for one participant
recode_gps_one_participant = function(id, mailing_coordinates){
  # pull in relevant files
  gps_file = Sys.glob(paste0("/Volumes/columbia/MAPS_Data/EARS_Features/GPS/", id, "/GPS_event_*.csv"))
  events = read.csv(gps_file)
  ears_home_location_file =  Sys.glob(paste0("/Volumes/columbia/MAPS_Data/EARS_Features/GPS/", id, "/GPS_person_location_home_*.csv"))
  daily_file =  Sys.glob(paste0("/Volumes/columbia/MAPS_Data/EARS_Features/GPS/", id, "/GPS_day*.csv"))
  daily = read.csv(daily_file)
  ears_home_location = read.csv(ears_home_location_file)

  # clean events file data
  events <- events %>% select(-is_invalid,-tm_gps_lead1,-val_lon_lead1,-val_lat_lead1,-sleep_interval)
  events <- events %>% mutate(across(c("val_lat","val_lon","val_horizontal_accuracy","amt_gps_duration_sec"),as.numeric))
  events$dt_gps <- ymd(events$dt_gps)
  
  #filter out null island (latitude and longitude 0 or very close to it)
  events <- events %>% filter(!((abs(val_lat) <=.01 & abs(val_lon) <=.01) &val_horizontal_accuracy>135))
  
  # add alternate home locations to events file
  events$home_lon_ears = ears_home_location$val_person_home_lon[1]
  events$home_lat_ears = ears_home_location$val_person_home_lat[1]
  
  # troubleshoot if no mailing coordinates are available
  if(id %in% mailing_coordinates$ID){
    events$home_lon_mailing = mailing_coordinates$val_long[mailing_coordinates$ID==id]
    events$home_lat_mailing = mailing_coordinates$val_lat[mailing_coordinates$ID==id]
  } else{
    events$home_lon_mailing = NA_real_
    events$home_lat_mailing = NA_real_
  }
  
  # find home location based on the entire study period (max sleep location)
  max_sleep_location_whole_study = get_max_sleep_location(gps_trace = events)
  events$home_lon_ears_wholestudy = max_sleep_location_whole_study$val_lon[1]
  events$home_lat_ears_wholestudy = max_sleep_location_whole_study$val_lat[1]
  
  dir.create(paste0('gps_recoded_files/', id))
  
  # write out home locations file
  all_home_locations = events[1,] %>% dplyr::select(contains('ears'))
  write.csv(all_home_locations, file = paste0('gps_recoded_files/', id, '/home_locations.csv'), row.names = FALSE)
  
  
  # clean daily gps data  
  daily$dt_gps <- ymd(daily$dt_feature)
  daily <- daily %>% select(dt_gps,amt_home_day_minutes, n_capture_day)
  #rename to clarify this amount homestay corresponds to ears
  daily$amt_home_day_mins_ears <- as.numeric(daily$amt_home_day_minutes)
  daily <- daily %>% select(-c(amt_home_day_minutes))
  
  # get distances for the gps trace from each home address (and betwen them)
  events = events %>% 
    rowwise %>% 
    mutate(# first, calculate differences between the different home locations
           dist_ears_home_location_from_mailing =distm(c(home_lon_ears, home_lat_ears), c(home_lon_mailing, home_lat_mailing), fun = distGeo)[1],
           dist_ears_home_location_from_ears_wholestudy = distm(c(home_lon_ears, home_lat_ears), c(home_lon_ears_wholestudy, home_lat_ears_wholestudy), fun = distGeo)[1],
           dist_mailing_home_location_from_ears_wholestudy = distm(c(home_lon_mailing, home_lat_mailing), c(home_lon_ears_wholestudy, home_lat_ears_wholestudy), fun = distGeo)[1],
           # now, calculate distance of each gps ping from each home location
           dist_from_home_ears =distm(c(home_lon_ears, home_lat_ears), c(val_lon, val_lat), fun = distGeo)[1],
           dist_from_home_mailing =distm(c(home_lon_mailing, home_lat_mailing), c(val_lon, val_lat), fun = distGeo)[1],
           dist_from_home_ears_wholestudy =distm(c(home_lon_ears_wholestudy, home_lat_ears_wholestudy), c(val_lon, val_lat), fun = distGeo)[1]) %>%
    ungroup()
  
  # code if home or not according to whether or not within 100m of each address (or any of them)
  events = events %>%
    mutate(home_ears = ifelse(events$dist_from_home_ears<100,1,0),
           home_mailing = ifelse(events$dist_from_home_mailing<100,1,0),
           home_ears_wholestudy = ifelse(events$dist_from_home_ears_wholestudy<100,1,0))

  if(id %in% mailing_coordinates$ID){
    events = events %>%
      mutate(home_either = ifelse(home_ears ==1 | home_mailing == 1 | home_ears_wholestudy ==1, 1, 0))
  }else{
    events = events %>%
      mutate(home_either = ifelse(home_ears ==1 | home_ears_wholestudy == 1, 1, 0)) 
    }
  
  # Calculate homestay on a daily level for each possible address (or the combination of all)
  calc = events %>% group_by(dt_gps) %>% summarize(total_min_avail=sum((amt_gps_duration_sec/60),na.rm=T), #raw gps data—every ping has length, sum them. maybe exclude days below a threshold of available gps data
                                                    homestay_min_ears=sum(home_ears*(amt_gps_duration_sec/60),na.rm=TRUE), # based on ears location
                                                    homestay_min_mailing=sum(home_mailing*(amt_gps_duration_sec/60),na.rm=TRUE), # based on mailing address
                                                    homestay_min_mailing = ifelse(sum(!is.na(home_mailing)) == 0, NA_real_, homestay_min_mailing),
                                                    homestay_min_ears_wholestudy=sum(home_ears_wholestudy*(amt_gps_duration_sec/60),na.rm=TRUE), # based on ears sleep location over whole study
                                                    homestay_min_either =sum(home_either*(amt_gps_duration_sec/60),na.rm=TRUE), # if any home location
                                                    n_pings_day = n(),
                                                    dist_ears_home_location_from_mailing =dist_ears_home_location_from_mailing[1],
                                                    dist_ears_home_location_from_ears_wholestudy =dist_ears_home_location_from_ears_wholestudy[1],
                                                    dist_mailing_home_location_from_ears_wholestudy=dist_mailing_home_location_from_ears_wholestudy[1]
                                                   ) %>% #how many pings per day (only pings if you move a certain threshold)) %>% 
    ungroup()
  
  # Make a descriptive plot of daily homestay at the various home locations over time for the participant
  homestay_plot = calc %>%
    pivot_longer(c('homestay_min_ears', 'homestay_min_mailing', 'homestay_min_ears_wholestudy')) %>%
    mutate(name = dplyr::recode(name,
                                'homestay_min_ears'='EARS Original',
                                'homestay_min_mailing'='Mailing Address', 
                                'homestay_min_ears_wholestudy'='EARS (From Whole Study)')) %>%
    ggplot(data = ., aes(x = dt_gps, y = value/60, color = name)) +
    geom_line() +
    geom_point(size = 0.5) +
    geom_hline(yintercept = 24, lty = 2) +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")  +
    labs(x = 'Date', y = 'Hours Homestay', color = 'Home Location')
  
  ggsave(homestay_plot,
         height = 4, width = 9, 
         file = paste0('gps_recoded_files/', id, '/homestay_over_time.png'))

  
  # join back together with daily
  daily = left_join(daily, calc, by = 'dt_gps')
  daily$ID = id
  
  # Write output csv
  write.csv(daily, file = paste0('gps_recoded_files/', id, '/gps_daily_recoded.csv'), row.names = FALSE)
  
  return(daily)
}

# Here is the real action! Run through all participants and recode homestay (will take a few hours)

for (participant in id_list){
  print(participant)
  tryCatch({
    # Attempt to run the function
    recode_gps_one_participant(id = participant, mailing_coordinates = mailing_coordinates)
    
  }, error = function(e) {
    # Print error message
    cat("Error occurred in attempt", participant, ":", conditionMessage(e), "\n")
  })
}




