
data("fips_codes")

county_y <- united_states_county_quarterly_deaths %>% 
  group_by(state_code = str_sub(region_code, end = 2), year, quarter) %>%
  summarise(across(c(population, total_deaths, covid_deaths), sum, na.rm = TRUE)) %>% 
  group_by(state_code, year) %>% 
  summarise(across(c(population), max), 
            across(c(total_deaths, covid_deaths), sum, na.rm = TRUE),
            .groups = "drop")

weekly_x <- united_states_weekly_deaths %>% 
  left_join(distinct(fips_codes, state, state_code), by = c("region_code" = "state"), keep = TRUE) %>% 
  group_by(state, state_code, year) %>% 
  summarise(across(c(week, population), max), 
            across(c(total_deaths, covid_deaths), sum, na.rm = TRUE),
            .groups = "drop")

compare <- weekly_x %>% 
  inner_join(county_y, by = c("state_code", "year"), suffix = c(".weekly", ".county"))

united_states_county_quarterly_results <- data.table::fread("output-data/excess-deaths/united_states_county_quarterly_results.csv", keepLeadingZeros = TRUE)

united_states_excess_deaths <- data.table::fread("output-data/excess-deaths/united_states_excess_deaths.csv", keepLeadingZeros = TRUE)

glimpse(united_states_county_quarterly_results)

united_states_county_quarterly_results %>% 
  summarise(across(c(total_deaths, covid_deaths, expected_deaths, excess_deaths, non_covid_deaths), sum, na.rm = TRUE))

county_outcome_y <- united_states_county_quarterly_results %>% 
  group_by(state_code = str_sub(region_code, end = 2)) %>% 
  summarise(across(c(total_deaths, covid_deaths, expected_deaths, excess_deaths, non_covid_deaths), sum, na.rm = TRUE))

weekly_outcome_x <- united_states_excess_deaths %>% 
  left_join(distinct(fips_codes, state, state_code), by = c("region_code" = "state"), keep = TRUE) %>% 
  filter(year == 2020) %>% 
  group_by(state, state_code) %>% 
  summarise(across(c(total_deaths, covid_deaths, expected_deaths, excess_deaths, non_covid_deaths), sum, na.rm = TRUE), .groups = "drop")

compare_outcome <- weekly_outcome_x %>% 
  left_join(county_outcome_y, by = "state_code", suffix = c(".weekly", ".county"))

united_states_county_quarterly_results %>% 
  ggplot(
    aes(
      x = excess_deaths_per_100k_per_7_days, 
      y = quarter, 
      group = quarter
      )
    ) + 
  geom_density_ridges(
    jittered_points = FALSE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    rel_min_height = 0.00001
  )

