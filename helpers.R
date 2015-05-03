year_to_date <- function(y) {
  as.Date(paste0(y, "-01-01"))
}

all_names <- ssa_national %>%
  group_by(name) %>%
  summarize(total = sum(female) + sum(male)) %>%
  filter(total > 1000) %>%
  select(name) %>%
  unlist(use.names = FALSE)
