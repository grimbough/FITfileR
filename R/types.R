# 
# fit_data_types <- list()
# 
# fit_data_types[[ 'activity' ]] <- data_frame(key = c(0,1), 
#                                         value = c("manual", "auto_multi_sport"))
# 
# 
# fit_data_types[[ 'activity' ]] <- data_frame(
#   key = c(0,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
#           24,25,26,27,28,32,33,36,42,43,44,45,46,47),
#   value = c("timer","workout","workout_step","power_down","power_up",
#             "off_course","session","lap","course_point","battery","virtual_partner_pace",
#             "hr_high_alert","hr_low_alert","speed_high_alert","speed_low_alert",
#             "cad_high_alert","cad_low_alert","power_high_alert","power_low_alert",
#             "recovery_hr","battery_low","time_duration_alert","distance_duration_alert",
#             "calorie_duration_alert","activity","fitness_equipment","length",
#             "user_marker","sport_point","calibration","front_gear_change",
#             "rear_gear_change","rider_position_change","elev_high_alert",
#             "elev_low_alert","comm_timeout"))
# 
# 
# 
# fit_data_types[[ 'event_type' ]] <- data_frame(
#   key = c(0:9),
#   value = c("start","stop","consecutive_depreciated","marker","stop_all",
#             "begin_depreciated","end_depreciated","end_all_depreciated",
#             "stop_disable","stop_disable_all"))
# 
# save(fit_data_types, file = "~/Code/Fit_files/fitFileR/data/fit_data_types.rda", compress = TRUE)
