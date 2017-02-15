value <- c("file_id","capabilities","device_settings","user_profile","hrm_profile",
           "sdm_profile","bike_profile","zones_target","hr_zone","power_zone","met_zone",
           "sport","goal","session","lap","record","event","device_info","workout",
           "workout_step","schedule","weight_scale","course","course_point","totals",
           "activity","software","file_capabilities","mesg_capabilities",
           "field_capabilities","file_creator","blood_pressure","speed_zone",
           "monitoring","training_file","hrv","ant_rx","ant_tx","ant_channel_id",
           "length","monitoring_info","pad","slave_device","connectivity",
           "weather_conditions","weather_alert","cadence_zone","hr","segment_lap",
           "memo_glob","segment_id","segment_leaderboard_entry","segment_point",
           "segment_file","watchface_settings","gps_metadata","camera_event",
           "timestamp_correlation","gyroscope_data","accelerometer_data",
           "three_d_sensor_calibration","video_frame","obdii_data","nmea_sentence",
           "aviation_attitude","video","video_title","video_description","video_clip",
           "ohr_settings","exd_screen_configuration","exd_data_field_configuration",
           "exd_data_concept_configuration","field_description","developer_data_id",
           "magnetometer_data")
key <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,18,19,20,21,23,26,27,28,30,31,32,33,
         34,35,37,38,39,49,51,53,55,72,78,80,81,82,101,103,105,106,127,128,
         129,131,132,142,145,148,149,150,151,159,160,161,162,164,165,167,169,
         174,177,178,184,185,186,187,188,200,201,202,206,207,208)

global_message_table <- data_frame(key, value)
