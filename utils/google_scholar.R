require("scholar")
library("rvest")

library("httr")
library("xml2")
library("rvest")
library("stringr")


naam = "Jaco Dagevos"
affiliation = "Erasmus University Rotterdam"

google_scholar.get_id = function(
    naam, affiliation = NA
  ){

  if (!is.character(naam)) {"naam should be a string"}
  author_name = str_replace(str_squish(naam), ' ', '+')
  
  # configure api call
  site = getOption("scholar_site")
  url = paste0(
      site,
      '/citations?view_op=search_authors&mauthors=',
      author_name,
      '&hl=en&oi=ao'
  )

  # Chrome on macOS (UA freeze uses 10_15_7 even on newer macOS versions)
  chrome_ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36"

  # resp <- request(url) |>
  #   req_user_agent(chrome_ua) |>
  #   req_timeout(20) |>
  #   req_retry(max_tries = 5) |>
  #   req_perform()

  httr::GET(url, user_agent(chrome_ua))
}

require("scholar")

get_scholar_id_fix <- function (last_name = "", first_name = "", affiliation = NA)
{
  if (!any(nzchar(c(first_name, last_name))))
    stop("At least one of first and last name must be specified!")
  site <- getOption("scholar_site")
  url <- paste0(site, "/citations?view_op=search_authors&mauthors=",
                first_name, "+", last_name, "&hl=en&oi=ao")
  page <- get_scholar_resp(url)
  if (is.null(page))
    return(NA)
  aa <- httr::content(page, as = "text")
  # added by Bas Hofstra: bugfix for IDs that have a dash ("-")
  ids <- substring(aa, regexpr("&user=", aa))
  ids <- substr(ids, 1, 19) # error prone, but unsure how to solve otherwise
  # if (nchar(stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]")[[1]][1]) < 18) {
  #   ids <- stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]+[[:alnum:]]+[[:punct:]]")
  # } else {
  #   ids <- stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]")
  # }
  if (length(unlist(ids)) == 0) {
    message("No Scholar ID found.")
    return(NA)
  }
  ids <- ids %>% unlist %>% gsub(";user=|[[:punct:]]$", "",
                                 .) %>% unique
  if (length(ids) > 1) {
    profiles <- lapply(ids, scholar::get_profile)
    if (is.na(affiliation)) {
      x_profile <- profiles[[1]]
      warning("Selecting first out of ", length(profiles),
              " candidate matches.")
    }
    else {
      which_profile <- sapply(profiles, function(x) {
        stringr::str_count(string = x$affiliation, pattern = stringr::coll(affiliation,
                                                                           ignore_case = TRUE))
      })
      if (all(which_profile == 0)) {
        warning("No researcher found at the indicated affiliation.")
        return(NA)
      }
      else {
        x_profile <- profiles[[which(which_profile !=
                                       0)]]
      }
    }
  }
  else {
    x_profile <- scholar::get_profile(id = ids)
  }
  return(x_profile$id)
}
