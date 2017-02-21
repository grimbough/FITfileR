# fit_global_messages <- data_frame(
#   key = c(0,1,2,3,4,5,6,7,8,9,10,12,15,18,19,20,21,23,26,27,28,30,31,32,33,
#            34,35,37,38,39,49,51,53,55,72,78,80,81,82,101,103,105,106,127,128,
#            129,131,132,142,145,148,149,150,151,159,160,161,162,164,165,167,169,
#            174,177,178,184,185,186,187,188,200,201,202,206,207,208),
#   value = c("file_id","capabilities","device_settings","user_profile","hrm_profile",
#            "sdm_profile","bike_profile","zones_target","hr_zone","power_zone","met_zone",
#            "sport","goal","session","lap","record","event","device_info","workout",
#            "workout_step","schedule","weight_scale","course","course_point","totals",
#            "activity","software","file_capabilities","mesg_capabilities",
#            "field_capabilities","file_creator","blood_pressure","speed_zone",
#            "monitoring","training_file","hrv","ant_rx","ant_tx","ant_channel_id",
#            "length","monitoring_info","pad","slave_device","connectivity",
#            "weather_conditions","weather_alert","cadence_zone","hr","segment_lap",
#            "memo_glob","segment_id","segment_leaderboard_entry","segment_point",
#            "segment_file","watchface_settings","gps_metadata","camera_event",
#            "timestamp_correlation","gyroscope_data","accelerometer_data",
#            "three_d_sensor_calibration","video_frame","obdii_data","nmea_sentence",
#            "aviation_attitude","video","video_title","video_description","video_clip",
#            "ohr_settings","exd_screen_configuration","exd_data_field_configuration",
#            "exd_data_concept_configuration","field_description","developer_data_id",
#            "magnetometer_data"))
# 
# save(fit_global_messages, file = "~/Code/Fit_files/fitFileR/data/fit_global_messages.rda", compress = TRUE)
# 
# 
# 
# 
# 
# fit_message_types <- list()
# 
# fit_message_types[[ 'record' ]] <- data_frame(
#   key = c(253,0,1,2,3,4,5,6,7,8,9,10,11,12,13,17,18,19,28,29,30,31,32,33,
#           39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,62,67,68,
#           69,70,71,72,73,78,81,82,83,84,85),
#   value = c("timestamp","position_lat","position_long","altitude","heart_rate",
#             "cadence","distance","speed","power","compressed_speed_distance","grade",
#             "resistance","time_from_course","cycle_length","temperature","speed_1s",
#             "cycles","total_cycles","compressed_accumulated_power","accumulated_power",
#             "left_right_balance","gps_accuracy","vertical_speed","calories",
#             "vertical_oscillation","stance_time_percent","stance_time","activity_type",
#             "left_torque_effectiveness","right_torque_effectiveness","left_pedal_smoothness",
#             "right_pedal_smoothness","combined_pedal_smoothness","time128","stroke_type","zone",
#             "ball_speed","cadence256","fractional_cadence","total_hemoglobin_conc",
#             "total_hemoglobin_conc_min","total_hemoglobin_conc_max","saturated_hemoglobin_percent",
#             "saturated_hemoglobin_percent_min","saturated_hemoglobin_percent_max","device_index",
#             "left_pco","right_pco","left_power_phase","left_power_phase_peak","right_power_phase",
#             "right_power_phase_peak","enhanced_speed","enhanced_altitude","battery_soc","motor_power",
#             "vertical_ratio","stance_time_balance","step_length"),
#   type = factor(c("date_time","sint32","sint32","uint16","uint8","uint8","uint32","uint16","uint16",
#                   "byte","sint16","uint8","sint32","uint8","sint8","uint8","uint8","uint32","uint16","uint32",
#                   "left_right_balance","uint8","sint16","uint16","uint16","uint16","uint16","activity_type",
#                   "uint8","uint8","uint8","uint8","uint8","uint8","stroke_type","uint8","uint16","uint16",
#                   "uint8","uint16","uint16","uint16","uint16","uint16","uint16","device_index","sint8","sint8",
#                   "uint8","uint8","uint8","uint8","uint32","uint32","uint8","uint16","uint16","uint16","uint16"))
# )
# 
# fit_message_types[[ 'lap' ]] <- data_frame(
#   key = c(254,253,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,32,33,34,35,
#           37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,71,74,75,
#           76,77,78,79,80,81,82,83,84,85,86,87,88,89,91,92,93,94,95,98,99,100,101,102,103,104,105,106,
#           107,108,109,110,111,112,113,114,115,116,117,118,119,120),
#   value = c("message_index","timestamp","event","event_type","start_time","start_position_lat",
#             "start_position_long","end_position_lat","end_position_long","total_elapsed_time",
#             "total_timer_time","total_distance","total_cycles/strides","total_calories",
#             "total_fat_calories","avg_speed","max_speed","avg_heart_rate","max_heart_rate","avg_cadence",
#             "max_cadence","avg_power","max_power",
#             "total_ascent","total_descent","intensity","lap_trigger","sport","event_group","num_lengths",
#             "normalized_power","left_right_balance","first_length_index","avg_stroke_distance",
#             "swim_stroke","sub_sport","num_active_lengths","total_work","avg_altitude","max_altitude",
#             "gps_accuracy","avg_grade","avg_pos_grade","avg_neg_grade","max_pos_grade","max_neg_grade",
#             "avg_temperature","max_temperature","total_moving_time","avg_pos_vertical_speed",
#             "avg_neg_vertical_speed","max_pos_vertical_speed","max_neg_vertical_speed","time_in_hr_zone",
#             "time_in_speed_zone","time_in_cadence_zone","time_in_power_zone","repetition_num",
#             "min_altitude","min_heart_rate","wkt_step_index","opponent_score","stroke_count",
#             "zone_count","avg_vertical_oscillation","avg_stance_time_percent","avg_stance_time",
#             "avg_fractional_cadence","max_fractional_cadence","total_fractional_cycles",
#             "player_score","avg_total_hemoglobin_conc","min_total_hemoglobin_conc",
#             "max_total_hemoglobin_conc","avg_saturated_hemoglobin_percent",
#             "min_saturated_hemoglobin_percent","max_saturated_hemoglobin_percent",
#             "avg_left_torque_effectiveness","avg_right_torque_effectiveness","avg_left_pedal_smoothness",
#             "avg_right_pedal_smoothness","avg_combined_pedal_smoothness","time_standing","stand_count",
#             "avg_left_pco","avg_right_pco","avg_left_power_phase","avg_left_power_phase_peak",
#             "avg_right_power_phase","avg_right_power_phase_peak","avg_power_position",
#             "max_power_position","avg_cadence_position","max_cadence_position","enhanced_avg_speed",
#             "enhanced_max_speed","enhanced_avg_altitude","enhanced_min_altitude","enhanced_max_altitude",
#             "avg_lev_motor_power","max_lev_motor_power","lev_battery_consumption","avg_vertical_ratio",
#             "avg_stance_time_balance","avg_step_length"),
#   type = factor(c("message_index","date_time","event","event_type","date_time","sint32","sint32",
#                   "sint32","sint32","uint32","uint32","uint32","uint32","uint16","uint16","uint16",
#                   "uint16","uint8","uint8","uint8","uint8","uint16","uint16","uint16",
#                   "uint16","intensity","lap_trigger","sport","uint8","uint16","uint16",
#                   "left_right_balance_100","uint16","uint16","swim_stroke","sub_sport","uint16","uint32",
#                   "uint16","uint16","uint8","sint16","sint16","sint16","sint16","sint16","sint8","sint8",
#                   "uint32","sint16","sint16","sint16","sint16","uint32","uint32","uint32","uint32",
#                   "uint16","uint16","uint8","message_index","uint16","uint16","uint16","uint16","uint16",
#                   "uint16","uint8","uint8","uint8","uint16","uint16","uint16","uint16","uint16","uint16",
#                   "uint16","uint8","uint8","uint8","uint8","uint8","uint32","uint16","sint8","sint8","uint8",
#                   "uint8","uint8","uint8","uint16","uint16","uint8","uint8","uint32","uint32","uint32",
#                   "uint32","uint32","uint16","uint16","uint8","uint16","uint16","uint16"))
# )
# 
# fit_message_types[[ 'file_id' ]] <- data_frame(
#   key = c(0,1,2,3,4,5,8),
#   value = c("type","manufacturer","product","serial_number",
#             "time_created","number","product_name"),
#   type = factor(c("file","manufacturer","uint16","uint32z","date_time","uint16","string"))
# )
# 
# fit_message_types[[ 'activity' ]] <- data_frame(
#   key = c(253,0,1,2,3,4,5,6),
#   value = c("timestamp","total_timer_time","num_sessions","type","event","event_type","local_timestamp","event_group"),
#   type = factor(c("date_time","uint32","uint16","activity","event","event_type","local_date_time","uint8"))
# )
# 
# fit_message_types[[ 'session' ]] <- data_frame(
#   key = c(254,253,0,1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,
#     25,26,27,28,29,30,31,32,34,35,36,37,41,42,43,44,45,46,47,48,49,50,51,52,53,
#     54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,82,83,84,85,86,87,88,
#     89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,111,112,113,114,115,
#     116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,137),
#   value = c("message_index","timestamp","event","event_type","start_time","start_position_lat",
#     "start_position_long","sport","sub_sport","total_elapsed_time","total_timer_time",
#     "total_distance","total_cycles/strides","total_calories","total_fat_calories",
#     "avg_speed","max_speed","avg_heart_rate","max_heart_rate","avg_cadence",
#     "max_cadence","avg_power","max_power",
#     "total_ascent","total_descent","total_training_effect","first_lap_index","num_laps",
#     "event_group","trigger","nec_lat","nec_long","swc_lat","swc_long","normalized_power",
#     "training_stress_score","intensity_factor","left_right_balance","avg_stroke_count",
#     "avg_stroke_distance","swim_stroke","pool_length","threshold_power",
#     "pool_length_unit","num_active_lengths","total_work","avg_altitude","max_altitude",
#     "gps_accuracy","avg_grade","avg_pos_grade","avg_neg_grade","max_pos_grade",
#     "max_neg_grade","avg_temperature","max_temperature","total_moving_time",
#     "avg_pos_vertical_speed","avg_neg_vertical_speed","max_pos_vertical_speed",
#     "max_neg_vertical_speed","min_heart_rate","time_in_hr_zone","time_in_speed_zone",
#     "time_in_cadence_zone","time_in_power_zone","avg_lap_time","best_lap_index",
#     "min_altitude","player_score","opponent_score","opponent_name","stroke_count",
#     "zone_count","max_ball_speed","avg_ball_speed","avg_vertical_oscillation",
#     "avg_stance_time_percent","avg_stance_time","avg_fractional_cadence",
#     "max_fractional_cadence","total_fractional_cycles","avg_total_hemoglobin_conc",
#     "min_total_hemoglobin_conc","max_total_hemoglobin_conc",
#     "avg_saturated_hemoglobin_percent","min_saturated_hemoglobin_percent",
#     "max_saturated_hemoglobin_percent","avg_left_torque_effectiveness",
#     "avg_right_torque_effectiveness","avg_left_pedal_smoothness",
#     "avg_right_pedal_smoothness","avg_combined_pedal_smoothness","sport_index",
#     "time_standing","stand_count","avg_left_pco","avg_right_pco","avg_left_power_phase",
#     "avg_left_power_phase_peak","avg_right_power_phase","avg_right_power_phase_peak",
#     "avg_power_position","max_power_position","avg_cadence_position",
#     "max_cadence_position","enhanced_avg_speed","enhanced_max_speed",
#     "enhanced_avg_altitude","enhanced_min_altitude","enhanced_max_altitude",
#     "avg_lev_motor_power","max_lev_motor_power","lev_battery_consumption",
#     "avg_vertical_ratio","avg_stance_time_balance","avg_step_length",
#     "total_anaerobic_training_effect"),
#   type = factor(c("message_index","date_time","event","event_type","date_time","sint32",
#            "sint32","sport","sub_sport","uint32","uint32","uint32","uint32",
#            "uint16","uint16","uint16","uint16","uint8","uint8","uint8",
#            "uint8","uint16","uint16","uint16","uint16","uint8","uint16","uint16","uint8",
#            "session_trigger","sint32","sint32","sint32","sint32","uint16","uint16",
#            "uint16","left_right_balance_100","uint32","uint16","swim_stroke","uint16",
#            "uint16","display_measure","uint16","uint32","uint16","uint16","uint8",
#            "sint16","sint16","sint16","sint16","sint16","sint8","sint8","uint32",
#            "sint16","sint16","sint16","sint16","uint8","uint32","uint32","uint32",
#            "uint32","uint32","uint16","uint16","uint16","uint16","string","uint16",
#            "uint16","uint16","uint16","uint16","uint16","uint16","uint8","uint8","uint8",
#            "uint16","uint16","uint16","uint16","uint16","uint16","uint8","uint8","uint8",
#            "uint8","uint8","uint8","uint32","uint16","sint8","sint8","uint8","uint8",
#            "uint8","uint8","uint16","uint16","uint8","uint8","uint32","uint32","uint32",
#            "uint32","uint32","uint16","uint16","uint8","uint16","uint16","uint16","uint8"))
# )
# 
# 
# save(fit_message_types, file = "~/Code/Fit_files/fitFileR/data/fit_message_types.rda", compress = TRUE)
# 

# library(openxlsx)
# t2 <- read.xlsx('~/projects/fit_files/Profile_gsheets.xlsx', sheet = 2)
# rm.idx <- which(is.na(t2[,1]) & is.na(t2[,2]) & is.na(t2[,3]))
# t3 <- t2[-rm.idx,1:4]
# names(t3) <- c("message_type", "key", "value", "type")
# t3[,'type'] <- as.factor(t3[,'type'])
# 
# idx <- which(!is.na(t3[,1]))
# t3[,1] <- rep(t3[idx,1], 
#               diff(c(idx, nrow(t3)+1)))
# t3 <- as_data_frame(t3[-which(is.na(t3[,2])),])
# 
# message_list <- split(t3[,2:4], t3[[1]])
