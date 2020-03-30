library(tidyverse)
library(RSocrata)

nyc <- c("Bronx", "Queens", "Staten Island", "Brooklyn")

# New York State Data from health.data.ny.gov

# Health Facility Certification Information
hospitals <- as_tibble(read.socrata("https://health.data.ny.gov/resource/2g9y-7kqm.csv"))
# Health Facility General Information
locations <- as_tibble(read.socrata("https://health.data.ny.gov/resource/vn5v-hh5r.csv"))

# Select only hospitals and filter out "Service" from attribute type.
hospitals <- hospitals %>%
  filter(description == "Hospital") %>% 
  filter(attribute_type == "Bed")

# Select only hospitals and the needed columns
locations <- locations %>%
  filter(description == "Hospital") %>%
  filter(city %in% nyc) %>%
  select(fac_id, facility_name, latitude, longitude)


