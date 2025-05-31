library(tidyverse)
library(lubridate)
dat <- read_csv("visit_data.csv")

dat <- dat %>%
  mutate(visit_date = dmy(visit_date))

dat %>% group_by(hn, visit_date) %>% dplyr::arrange(visit_date, visit_number) -> dat

write_csv(dat, "visit_data.csv")


