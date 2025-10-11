# clear the global environment
rm(list = ls())
gc()

#load custom functions
source("src/utils/custom_functions.r")

fpackage.check(c(
  'tidyverse', 'readxl',  'stringr', 
  'lubridate', 'httr2', 'rvest', 'xml2',
  "purrr"
))

is_ok = function(resp) resp_status(resp) >= 200 && resp_status(resp) < 300

request_last_name = function(base_url){
  # configure user agent
  ua = paste(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 15_5)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/129.0.0.0 Safari/537.36"
  )

  req = request(base_url) |>
    req_user_agent(ua) |>
    # disable SSL verification
    req_options(ssl_verifypeer = 0) |>
    req_timeout(30) |>
    req_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "nl,en;q=0.8"
    ) |>
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
    res = list(ok = FALSE, status = NA_integer_, url = url, resp = NULL, error = resp)
  } else {
    res = list(ok = is_ok(resp), status = resp_status(resp), url = url, resp = resp, error = NULL)
  }
  return(res)
}


format_url = function(
    name,
    base = "https://www.cbgfamilienamen.nl/nfb/detail_naam.php?",
    .what = "base"
  ){
  # format_name for URL
  formatted_name = name |> 
    URLencode(reserved = TRUE) |>
    str_replace_all(pattern = "%20", '+')

  # configure url
  url = paste0(
    base,
    "gba_naam=", formatted_name,
    "&gba_lcnaam=", tolower(formatted_name),
    "&nfd_naam=", formatted_name
  )

  if (.what == 'info'){
    url = paste0(url, "&info=analyse+en+verklaring")
  }
  
  return(url)
}

extract_info = function(r){
  html = read_html(resp_body_string(r$resp))
  text = html |>
    html_element("body") |>
    html_text()

  info = c("")
  if (str_detect(text, regex('kenmerken:|verklaring:'))){
    parts = str_split_fixed(text, pattern=regex('kenmerken:|verklaring:'), 2)[2] |>
      str_split(pattern = regex("[\\n|\\t|\\s]{3,20}")) |>
      unlist()

    idx = which(grepl(regex("©"), parts))

    if (length(idx) >= 1) {
      info = parts[seq_len(idx[1] - 1)]
      info = as.vector(info[nzchar(info)])
    }
  }

  # drop empty strings
  # info = info[nzchar(info)]
  return(paste(unname(info), collapse = "; "))
}

extract_count = function(r){
  # extract all tables on the page
  html = read_html(resp_body_string(r$resp))
  tables = html_table(html, header=FALSE)

  # select the first table if a table if found
  if (length(tables) == 0) stop("No tables were found")
  
  # set count value
  count = NA_integer_
  if (length(tables) >= 4) {
    for (tab in tables){
      i = nrow(tab)
      if (i >= 5){
        count = tab[2,2] |> pull()
      }
    }
  }

  return(count)
}

# helpers that return NA on any error/warning
safe_count = function(r, default = NA_integer_) {
  tryCatch(extract_count(r),
           error = function(e) default,
           warning = function(w) default)
}

safe_info = function(r, default = NA_character_) {
  tryCatch(extract_info(r),
           error = function(e) default,
           warning = function(w) default)
}

get_name_row = function(name, count=NA_character_, info=NA_character_){
  # scrape the count information
  r1 = format_url(name) |>
    request_last_name() 

  count = if (isTRUE(r1$ok)) safe_count(r1) else NA_character_
  count = as.character(count)

  # scrape info if the first scrape yielded success
  if (!is.na(count)){
  r2 = format_url(name, .what="info") |>
    request_last_name()

  info = if (isTRUE(r2$ok) && isTRUE(r1$ok)) safe_info(r2) else NA_character_
  info = as.character(info)
  }


  as_tibble(list(
    last_name = name, 
    name_count = count,
    info = info
  ))
}


dir = file.path('data', 'processed')
file = list.files(dir, pattern = 'names.Rds')[[1]]
names = readRDS(file.path(dir, file))

last_names = names |>
  distinct(particle, last_name) |>
  unite(last_name, particle:last_name, sep=" ", na.rm=TRUE) |>
  filter(!is.na(last_name), last_name != "") |>
  pull(last_name) |>
  sort()

ethnicity_cache = readRDS(file.path('data', 'utils', 'cbg_cache.Rds'))

hold = c()
for (name in last_names){
  if (!name %in% ethnicity_cache$last_name){
    hold[[name]] = get_name_row(name)
  }
}

ethnicity_cache = bind_rows(ethnicity_cache, bind_rows(hold))
saveRDS(ethnicity_cache,  file.path('data', 'utils', 'cbg_cache.Rds'))


