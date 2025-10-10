rm(list = ls())
gc()

source("src/utils/custom_functions.r")

# load and activate packages
fpackage.check(c(
  'tidyverse', 'readxl',  'stringr', 
  'lubridate', 'httr2', 'rvest', 'xml2',
  "purrr"
))

is_ok = function(resp) resp_status(resp) >= 200 && resp_status(resp) < 300

request_gender = function(
    first_name,
    base = "https://nvb.meertens.knaw.nl/naam/is/",
    pause = 0.5
  ){
  # configure url for scraping
  url  = paste0(base, URLencode(tolower(first_name), reserved = TRUE))

  # configure user agent
  ua = paste(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 15_5)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/129.0.0.0 Safari/537.36"
  )

  req = request(url) |>
    req_user_agent(ua) |>
    req_timeout(30) |>
    req_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "nl,en;q=0.8"
    ) |>
    # Retry on 429/5xx, and *also* on network hiccups:
    req_retry(
      max_tries = 4,
      backoff = ~ runif(1, 0.5, 1.2) * (2 ^ (.x - 1)),  # jittered exponential
      is_transient = function(resp) {
        code <- resp_status(resp)
        isTRUE(code == 429L || (code >= 500L & code < 600L))
      }
    )

  # polite pause + jitter
  if (pause > 0) Sys.sleep(pause + runif(1, 0, 0.4))

  # CRITICAL: don't throw on transport errors
  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      attr(e, "nvb_url") <- url
      e
    }
  )

  # Return a uniform list the caller can inspect
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, status = NA_integer_, url = url, resp = NULL, error = resp))
  }

  list(ok = is_ok(resp), status = resp_status(resp), url = url, resp = resp, error = NULL)
}

extract_gender_information = function(
    resp,
    first_name
  ){
  # extract all the tables on the page
  html = read_html(resp_body_string(resp))
  tables = html_table(html, header=TRUE)

  # select the first table if a table if found
  if (length(tables) == 0) stop("No tables were found")
  tab = tables[[1]]

  # extract information from table
  male_count = tab[1, 3] |> pull()
  male_count = ifelse(male_count == '--', 0, as.numeric(male_count))
  female_count = tab[5, 3] |> pull() 
  female_count = ifelse(female_count == '--', 0, as.numeric(female_count))
  probability_male = male_count / (female_count + male_count) 

  # configure results table
  res = tibble::tribble(
    ~first_name, ~male_count, ~female_count, ~probability_male,
    first_name, male_count,  female_count,  probability_male
  )
  return(res)
}

safe_extract = purrr::possibly(
  extract_gender_information,
  otherwise = tibble::tibble(
    first_name       = NA_character_,
    male_count       = NA_integer_,
    female_count     = NA_integer_,
    probability_male = NA_real_
  )
)

get_gender_row = function(name, gender) {
  # If cached, return from cache
  if (name %in% gender$first_name) {
    return(gender |> filter(first_name == name))
  }

  r <- request_gender(name)

  # If transport error or HTTP not OK, surface status & keep going
  if (!isTRUE(r$ok)) {
    return(tibble(
      first_name       = NA_character_,
      male_count       = NA_integer_,
      female_count     = NA_integer_,
      probability_male = NA_real_,
      status           = r$status
    ))
  }

  out <- safe_extract(r$resp, name) |>
    mutate(status = r$status, first_name = name)

  out
}


patch_gender_on_splits = function(gender){
  # set gender cache
  gender_cache = gender |> drop_na()
  # select authors without gender, and split their names
  selection = gender |>
    filter(is.na(male_count)) |>
    mutate(first_name_split = str_split(first_name, ' ')) |>
    unnest_longer(first_name_split) |>
    select(first_name, first_name_split)

  # get first names from selection
  first_names = selection |>
    select(first_name_split) |>
    pull() |>
    unique()

  # patch gender --------------------------------------
  gender_patch = purrr::map_dfr(
    first_names, get_gender_row, gender = gender_cache
  )

  # aggregate gender information
  gender_patch = selection |>
    left_join(
      gender_patch, 
      by=join_by(first_name_split == first_name)
    ) |>
    drop_na() |>
    # take the average gender count and probablity
    # for names where both splits yielded a gender
    # result
    group_by(first_name) |>
    summarise(
      male_count = as.integer(mean(male_count)),
      female_count = as.integer(mean(female_count)),
      probability_male = mean(probability_male)
    ) |>
    ungroup() |>
    mutate(status = 200)

  gender |> rows_update(gender_patch)
}

dir = file.path('data', 'processed')
file = list.files(dir, pattern = 'names.Rds')[[1]]
names = readRDS(file.path(dir, file))

first_names = names |>
  distinct(first_name) |>
  filter(!is.na(first_name), first_name != "") |>
  pull(first_name)

gender_cache = readRDS(file.path("data", "utils", "nvb_gender.Rds")) |> 
  select(-status) |>
  tidyr::drop_na()

gender = purrr::map_dfr(
    first_names, 
    get_gender_row, 
    gender = gender_cache
  ) |>
  patch_gender_on_splits() |>
  select(-status)

saveRDS(
  gender |> drop_na(),
  file.path('data', 'utils', "nvb_gender.Rds")
)

fsaveRDS(gender, 'gender')


# read_gender_io_source = function(
#     gender,
#     file = file.path('data', 'utils', 'all_genderize_io.csv')
#   ){
#   # load genderized data 
#   # data was sourced from:
#   # https://raw.githubusercontent.com/GenderGapSTEM-PublicationAnalysis/name_gender_inference/refs/heads/main/name_gender_inference/test_data/gender_api_full/all_gender_api_full.csv
#   gender_source = read.csv(file) |>
#     select(first_name, api_count, api_probability, api_gender) |>
#     drop_na()

#   # get the names for whom we have no data
#   first_names = gender |> 
#     filter(is.na(probability_male)) |> 
#     select(first_name) |> pull()

#   # loop over first_names
#   hold = c()
#   for (name in first_names){
#     temp = gender_source |> filter(first_name == tolower(name))

#     hold[[name]] = temp |>
#       mutate(first_name = name) |>
#       head(1)
#   }
# }



# hold = c()
# for (name in first_names){
#   temp = gender_source |> filter(first_name == tolower(name))

#   hold[[name]] = temp |>
#     mutate(first_name = name) |>
#     head(1)
# }

# gender |>
#   filter(is.na(probability_male)) |>
#   left_join(bind_rows(hold)) |>
#   drop_na(api_count) |>
#   mutate(
#     male_count = ifelse(api_gender == 'male', api_count, NA),
#     female_count = ifelse(api_gender == 'female', api_count, NA),
#     probability_male = ifelse(api_gender == 'male', api_count, 1 - api_count)
#   )
