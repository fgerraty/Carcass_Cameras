
# Apply 10min temporal filter to nighttime detections ####

assign_events <- function(times, window_min) {
  n <- length(times)
  event_id <- integer(n)
  event_id[1] <- 1
  event_start <- times[1]
  current <- 1
  
  if (n > 1) {
    for (i in seq_len(n)[-1]) {
      if (as.numeric(difftime(times[i], event_start, units = "mins")) < window_min) {
        event_id[i] <- current
      } else {
        current <- current + 1
        event_start <- times[i]
        event_id[i] <- current}}}
  event_id
}

independent_nocturnal <- carcass_camera_data |> 
  filter(str_detect(file_name, "MT")) |> 
  filter(event_type == "scavenging") |>  
  arrange(ccam_num, deployment, species_1, date_time)  |> 
  group_by(ccam_num, deployment, species_1) |> 
  mutate(event_id = assign_events(date_time, 10)) |> 
  group_by(ccam_num, deployment, species_1, event_id) |> 
  mutate(n_lumped = n(), lumped = n_lumped > 1,
         lumped_files = paste(file_name, collapse = "; ")) |> 
  slice_max(count, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  select(-event_id)

#Daytime Processing ####