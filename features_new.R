# Convert quaternion to euler 
quaternion_to_euler <- function(x,y,z,w){
  t0 = +2.0 * (w * x + y * z)
  t1 = +1.0 - 2.0 * (x * x + y * y)
  X = atan2(t0, t1)*180/pi
  
  t2 = +2.0 * (w * y - z * x)
  t2 = ifelse(t2 > 1, 1, t2)
  t2 = ifelse(t2 < -1, -1, t2)
  Y = asin(t2)*180/pi
  
  t3 = +2.0 * (w * z + x * y)
  t4 = +1.0 - 2.0 * (y * y + z * z)
  Z = atan2(t3, t4)*180/pi
  
  return(c(X, Y, Z))
}


generate_features <- function(df){
  
  ## Quaternion to euler
  eulers <- quaternion_to_euler(df$orientation_X, df$orientation_Y, df$orientation_Z, df$orientation_W) 
  
  x <- seq(1, length(eulers)/3)
  y <- seq(length(eulers)/3 + 1, length(eulers)*2/3)
  z <- seq(length(eulers)*2/3 + 1, length(eulers))
  
  df$euler_X <- eulers[x]
  df$euler_Y <- eulers[y]
  df$euler_Z <- eulers[z]

  feature_set1 <- df %>% 
    select(c(series_id, measurement_number , cols, euler_X, euler_Y, euler_Z)) %>% 
    mutate(orientation = sqrt(orientation_X**2 + orientation_Y**2 + orientation_Z**2),
           angular_velocity = sqrt(angular_velocity_X**2 + angular_velocity_Y**2 + angular_velocity_Z**2),
           linear_acceleration = sqrt(linear_acceleration_X**2 + linear_acceleration_Y**2 + linear_acceleration_Z**2),
           acceleration_velocity = linear_acceleration/angular_velocity) %>% 
    gather(entity, value, -measurement_number, -series_id) %>% 
    arrange(series_id, entity, measurement_number) %>% 
    group_by(series_id, entity) %>% 
    mutate(change = value - lag(value),
           change_change = change - lag(change)) %>% 
    summarise(avg = mean(value),
              min = min(value),
              max = max(value),
              std = sd(value),
              avg_abs = mean(abs(value)),
              max_abs = max(abs(value)),
              avg_change = mean(change, na.rm = T),
              min_change = min(change, na.rm = T),
              max_change = max(change, na.rm = T),
              std_change = sd(change, na.rm = T),
              avg_abs_change = mean(abs(change), na.rm = T),
              max_abs_change = max(abs(change), na.rm = T),
              avg_change_change = mean(change_change, na.rm = T),
              min_change = min(change_change, na.rm = T),
              max_change = max(change_change, na.rm = T),
              std_change = sd(change_change, na.rm = T),
              avg_abs_change_change = mean(abs(change_change), na.rm = T),
              max_abs_change_change = max(abs(change_change), na.rm = T)) %>% 
    mutate(min_max = max/min,
           range = (max - min)/std) %>% 
    gather(variable, value, (avg:range)) %>% 
    unite(temp, entity, variable) %>%
    spread(temp, value)
  
  feature_set2 <- df %>% 
    select(c(measurement_number, series_id, cols)) %>% 
    gather(activity, key, -measurement_number, -series_id) %>% 
    group_by(series_id, activity) %>% 
    mutate(ma=rollapply(key,10,mean,align='right',fill=NA)) %>% 
    group_by(series_id, activity) %>% 
    summarise(ma_avg = mean(ma, na.rm =T),
              ma_min = min(ma, na.rm =T),
              ma_max = max(ma, na.rm =T),
              ma_std = sd(ma, na.rm =T),
              ma_avg_abs = mean(abs(ma), na.rm =T),
              ma_max_abs = max(abs(ma), na.rm =T)) %>% 
    gather(variable, key, (ma_avg:ma_max_abs)) %>% 
    unite(temp, activity, variable) %>%
    spread(temp, key)
    
  features <- feature_set1 %>% 
    inner_join(feature_set2) %>% 
    as.data.frame()
  
  return(features)
}  
  