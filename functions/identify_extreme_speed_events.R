# modified from tgc::identify_heat_stress_events

identify_extreme_speed_events <- function(dat) {

  ints <- dat %>%
    filter(extreme) %>%
    group_by(deployment_id, station) %>%
    arrange(timestamp_utc, .by_group = TRUE) %>%
    mutate(
      interval_start = timestamp_utc,
      interval_end = timestamp_utc + minutes(15),

      # overlap with previous interval?
      overlap_lag = interval_start <= dplyr::lag(interval_end),
      # first observation cannot over lap with previous
      overlap_lag = if_else(
        interval_start == min(interval_start), FALSE, overlap_lag
      ),

      # overlap with next interval?
      overlap_lead = dplyr::lead(interval_start) <= interval_end,
      # last observation cannot over lap with next
      overlap_lead = if_else(
        interval_start == max(interval_start), FALSE, overlap_lead
      ),

      int_id = case_when(

        overlap_lag == FALSE & overlap_lead == FALSE ~ 99, # single obs event
        overlap_lag == FALSE & overlap_lead == TRUE ~ 1,   # beginning of event
        overlap_lag == TRUE & overlap_lead == TRUE ~ 2,    # middle of event
        overlap_lag == TRUE & overlap_lead == FALSE ~ 3    # end of event

      ),

      int_id_lead = lead(int_id)
    ) %>%
    ungroup()

  events_out <- list()

  k <- 1 # id counter

  for(i in seq_along(1:nrow(ints))){

    int.i <- ints[i, ]

    # stop with error if any id combos that should not exist
    # e.g., can't have 99 (no overlaps) followed by 2 (overlap with previous and next interval)
    if(!is.na(int.i$int_id_lead)){
      with(
        int.i,
        if(int_id == 99 && int_id_lead == 2 |
           int_id == 99 && int_id_lead == 3 |
           int_id == 1 && int_id_lead == 1 |
           int_id == 1 && int_id_lead == 99 |
           int_id == 2 && int_id_lead == 1 |
           int_id == 2 && int_id_lead == 99 |
           int_id == 3 && int_id_lead == 2 |
           int_id == 3 && int_id_lead == 3 ){

          stop("Problem identifying heat stress events.
               \n HINT: If dat has a STATION column, make sure STATION is
               defined as a grouping variable in identify_heat_stress_events()")

        }
      )
    }

    # assign event id
    int.i$event_id <- k

    events_out[[i]] <- int.i

    # update id at the end of event
    if(int.i$int_id == 99 | int.i$int_id == 3) k <- k + 1

    # reset id at the end of the group
    if(is.na(int.i$int_id_lead)) k <- 1

  }

  events_out %>%
    map_df(rbind) %>%
    # find first and last timestamp of each event
    dplyr::group_by(deployment_id, station, event_id) %>%
    dplyr::summarise(
      event_start = min(interval_start),
      event_end = max(interval_end),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(deployment_id, station, event_id, event_start, event_end)

}
