
process_data <- function(data_to, is_train) {
  

  print("add time ...")
  
  data <- copy(data_to)
  data[, time := 60 * hour + minute + (1 / 60) * second]
  
  
  print("add delta ...")
  data[, delta_userapp_bc := data.table::shift(time, fill = 60 * 24 * 3) - time,
       by = .(ip, app, device, os)]
  
  data[, delta_user_bc := data.table::shift(time, fill = 60 * 24 * 3) - time,
       by = .(ip, app, os)]
  
  
  data[, delta_userapp_fw :=
         data.table::shift(delta_userapp_bc, type = "lead", fill = 60 * 24 *
                             3)]
  
  data[, delta_user_fw :=
         data.table::shift(delta_user_bc, type = "lead", fill = 60 * 24 *
                             3)]
  
  print("delete time and second ...")
  data[, time := NULL]
  data[, second := NULL]
  
  ##
  # most_freq_hours_in_test_data <- c(4, 5, 9, 10, 13, 14)
  # least_freq_hours_in_test_data <- c(6, 11, 15)
  
  
  ##
  print("add nip_day ...")
  # data[,
  #      in_test_hh :=
  #        ifelse(
  #          hour %in% most_freq_hours_in_test_data,
  #          1,
  #          ifelse(hour %in% least_freq_hours_in_test_data, 3, 2)
  #        )]
  # data[,
  #      "nip_day_test_hh" := .N,
  #      by = .(ip, day, in_test_hh)]
  # data[, "in_test_hh" := NULL]
  
  
  ##
  print("aggregating ...")
  
  data[, "n_ip_hour" := .N,
       by = .(ip, hour)]
  
  
  data[, "n_ip_os_hour" := .N,
       by = .(ip, hour, os)]
  
  data[, "n_ip_app_hour" := .N,
       by = .(ip, hour, app)]
  
  data[, "n_ip_device_hour" := .N,
       by = .(ip, hour, device)]
  
  data[, "n_ip_app_os_hour" := .N,
       by = .(ip, hour, app, os)]
  
  data[, "n_ip_app_device_hour" := .N,
       by = .(ip, hour, app, device)]
  
  data[, "n_ip_channel_hour" := .N,
       by = .(ip, hour, channel)]
  

  
  data[, "n_app_hour" := .N,
       by = .(app, hour)]
  
  data[, "n_oss_hour" := .N,
       by = .(os, hour)]
  
  #data[, "n_device_hour" := .N,
  #      by = .(device, hour)]
  
  #data[, "n_channel_hour" := .N,
  #      by = .(device, hour)]
  
  
  ##
  print("add time var and mean ...")
  
  
  data[, "var_hour_by_ip_channel":=var(hour),
       by = .(ip, day, channel)]

  data[, "var_hour_by_ip_app_os":=var(hour),
       by = .(ip, day, app, os)]

  data[, "mean_hour_by_ip_app_channel":=mean(hour),
       by = .(ip, app, channel)]

  
  ##
  print("add sequence ...")
  data[, "nth_ip_os_device" := 1:.N,
       by = list(ip, device, os)]
  
  data[, "nth_ip_app_os_device" := 1:.N,
       by = list(ip, app, device, os)]
  
  
  ## add global aggregatin restrected test hour
  print("add global aggregatin restricted test hour ...")
  
  
  if (is_train == T) {
    ## for train data
    
    #cl_data <- colnames(data)
    
    n_ip_os_device <-
      data[hour %in% 0:15, .("n_ip_os_device" = .N),
           by = list(ip, device, os)]
    
    
    n_ip_app_os_device <-
      data[hour %in% 0:15, .("n_ip_app_os_device" = .N),
           by = .(ip, app, device, os)]
    
    n_ip_app_channel <- 
      data[hour %in% 0:15, .("n_ip_app_channel" = .N),
         by = .(ip,  app, channel)]
    
    
    data <-
      merge(data,
            n_ip_os_device,
            by = c("ip", "device", "os"),
            all.x = T)
    data <-
      merge(
        data,
        n_ip_app_os_device,
        by = c("ip", "app", "device", "os"),
        all.x = T
      )
    
    data <-
      merge(
        data,
        n_ip_app_channel,
        by = c("ip", "app", "channel"),
        all.x = T
      )
    
    
    
    rm(n_ip_os_device, n_ip_app_os_device, n_ip_app_channel)
    
    
    
    col_na <- 
    data %>% 
      map_lgl(~any(is.na(.x)))
    
    
    for(ii in names(col_na)[col_na]) {
      data[is.na(get(ii)), ii := 0, with = F]
      }
    
    
    
  } else {
    ## for test data
    data[, "n_ip_os_device" := .N,
         by = list(ip, device, os)]
    
    data[, "n_ip_app_os_device" := .N,
         by = .(ip, app, device, os)]
    
    data[, "n_ip_app_channel" := .N,
         by = .(ip,  app, channel)]
    
  }
  
  
  return(data)
}
