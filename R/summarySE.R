summarySE <- function(data, 
                      measurevar, 
                      groupvars = NULL, 
                      na.rm = TRUE,
                      conf.interval = 0.95) {
  
  if (!is.null(groupvars)) {
    out <- data |> group_by(across(all_of(groupvars)))
  } else {
    out <- data
  }
  
  # ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  # datac$ci <- datac$se * ciMult
  
  purrr::map_dfr(
    measurevar,
    ~ out |> 
      reframe(
        n = n(),
        mean = mean(.data[[.x]], na.rm = na.rm),
        sd = sd(.data[[.x]], na.rm = na.rm),
        se = sd / sqrt(n),
        ci = se * (qt(conf.interval/2 + 0.5, n-1))
      ) |> 
      mutate(
        type = .x
      )
  ) |> 
    mutate(
      across(
        is.numeric,
        ~ round(.x, 2)
      )
    )
}