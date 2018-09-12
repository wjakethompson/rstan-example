### User input -----------------------------------------------------------------
num_resp <- 1000
num_item <- 30


### Setup ----------------------------------------------------------------------
needed_packages <- c("tidyverse", "here")
load_packages <- function(x) {
  if (!x %in% installed.packages()) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
vapply(needed_packages, load_packages, logical(1))


### Define functions -----------------------------------------------------------
logit <- function(x) {
  log(x / (1 - x))
}
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


### Generate data --------------------------------------------------------------
set.seed(9416)
respondents <- data_frame(
  respondent_id = seq_len(num_resp),
  theta = rnorm(n = num_resp, mean = 0, sd = 1)
) %>%
  write_rds(here("data/respondent-parameters.rds"), compress = "gz") %>%
  write_csv(here("data/respondent-parameters.csv"))
  

items <- data_frame(
  item_id = seq_len(num_item),
  b = rnorm(n = num_item, mean = 0, sd = 0.8),
  a = rlnorm(n = num_item, meanlog = 0, sdlog = 0.4),
  c = rbeta(n = num_item, shape1 = 75, shape2 = 255)
) %>%
  write_rds(here("data/item-parameters.rds"), compress = "gz") %>%
  write_csv(here("data/item-parameters.csv"))

response_data <- map_dfr(respondents$theta, function(x, items) {
  items %>%
    mutate(
      prob = c + (1 - c) * inv_logit(a * (x - b)),
      rand = runif(n = nrow(.), min = 0, max = 1),
      miss = runif(n = nrow(.), min = 0, max = 1),
      score = case_when(
        miss <= 0.05 ~ NA_integer_,
        rand <= prob ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    select(score) %>%
    rowid_to_column(var = "item_id")
},
  items = items, .id = "student_id") %>%
  mutate(student_id = as.integer(student_id)) %>%
  spread(key = item_id, value = score) %>%
  write_rds(here("data/sample-data.rds"), compress = "gz") %>%
  write_csv(here("data/sample-data.csv"))
