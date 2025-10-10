library(tidyverse) #groundhog
library(readxl) #renv
library(janitor)
library(lubridate)
source(file.path("utils", 'google_scholar.R'))

read_spreadsheet = function(filepath, sheet){  
  readxl::read_excel(filepath, sheet = sheet) 
}

harmonize_columns = function(data, discipline, date){
  data = data |>
    clean_names() |>
    mutate(
      discipline = discipline,
      date = date
    ) |>
    relocate(c(discipline, date), .before = 1)
}

fread = function(
    files, 
    source,
    disciplines = c( "Politicologie", "Sociologie"),
    language = 'nl'
  ){
  # read in both sheets from excel files
  hold = c()
  for (file in files){
    filepath = file.path(source, file)
    date = ymd(str_split(file, pattern = "_")[[1]][1])

    # each file contains two sheets, for each discipline
    # read in both sheets and combine these data
    disciplines = c( "Politicologie", "Sociologie")
    chunks = c()
    for (discipline in disciplines){
      chunk = read_spreadsheet(filepath, discipline) |>
        harmonize_columns(discipline, date)

      chunks[[discipline]] = chunk
    }

    hold[[file]] =  bind_rows(chunks)
  }

  data = bind_rows(hold)

  if (!language %in% c('nl', 'en')) stop("language should be nl or en")
  # implement rename function for english column namesac
  

  return(data)
}


fix_scholar_names = function(data){
  # fix the particles in the names
  particles = c("van", "de", "den", "der", "te", "ten", "ter")
  pattern = regex(
    paste0("(?<=\\s)(", paste(particles, collapse = "|"), ")(?=\\s)"),
    ignore_case = TRUE
  )

  data |>
    mutate(
      # replace capitalized particles
      naam = str_replace_all(naam, pattern, ~ tolower(.x)),

      # replace typos and mistakes in names
      naam = case_when(
        naam == "Andrea Forstrer"      ~ "Andrea Forster",
        naam == "AJGM van Montfort"    ~ "A.J.G.M. van Montfort",
        naam == "FP Wagenaar"          ~ "F.P. Wagenaar",
        naam == "JP Presley"           ~ "J.P. Presley",
        naam == "JS Timmer"            ~ "J.S. Timmer",
        naam == "ilya Lavrov"          ~ "Ilya Lavrov",
        naam == "p. Vila Soler"        ~ "P. Vila Soler",
        naam == "Z Dong"               ~ "Z. Dong",
        naam == "Renae  Loh"           ~ "Renae Loh",
        naam == "Paulina Pankowski"    ~ "Paulina Pankowska",
        naam == "M.M Cuperus"          ~ "M.M. Cuperus",
        naam == "Lea Kroner"           ~ "Lea Kröner",
        naam == "L Slot"               ~ "L. Slot",
        naam == "Jan Willen Duyvendak" ~ "Jan Willem Duyvendak",
        .default = naam
      )
    )
}

fix_google_scholar_id = function(data, .direction='updown'){
  data |>
    arrange(naam, date) |>
    group_by(naam) |>
    # fill missing values with available information
    fill(google_scholar_id, .direction = .direction) |>  
    ungroup() |>
    arrange(universiteit, date)
}
# openaire : in plaats van narcis

fix_email_adresses = function(data, .direction = "updown"){
  # practical email regex (case-insensitive), supports subdomains
  email_pattern = regex(
    "\\b[[:alnum:]._%+-]+@[[:alnum:]-]+(?:\\.[[:alnum:]-]+)+\\b", 
    ignore_case = TRUE
  )

  # clean email variable
  data |>
    mutate(
      email_adres = str_extract(
        email_adres, email_pattern
        ) |> tolower()
    ) |>
    group_by(universiteit, naam) |>
    # fill 
    fill(email_adres, .direction = "updown") |>
    ungroup()|>
    arrange(universiteit, date)
}

clean_universities = function(data){
  universities = c("EUR", "RU", "RUG", "UU", "VU", "UvA", "UvT")
  pat   = regex("\\b(EUR|RU|RUG|UU|VU|UvA|UvT)\\b", ignore_case = TRUE)
  canon = setNames(universities, universities)

  # clean universities
  data |>
    # split university strings on '\s' , '/', '\.', and '?'
    mutate(
      universiteit = str_split(universiteit, "/+|\\.+|\\?+")
    ) |>
    unnest_longer(universiteit) |>
    # clean the university labels
    mutate(
      universiteit = str_replace(universiteit, pat, \(m) canon[str_to_lower(m)]),
      universiteit = case_when(
        (is.na(universiteit) & str_detect(email_adres, 'essb.eur.nl')) ~ 'EUR',
        (is.na(universiteit) & str_detect(email_adres, 'vu.nl'))  ~ 'VU',
        (is.na(universiteit) & str_detect(email_adres, 'uva.nl'))  ~  'UVA',
        (is.na(universiteit) & str_detect(email_adres, 'leidenuni'))  ~  'Leiden',
        (is.na(universiteit) & str_detect(email_adres, 'ru.nl'))  ~  'RU',
        (is.na(universiteit) & str_detect(email_adres, 'rug.nl'))  ~  'RUG',
        (is.na(universiteit) & str_detect(email_adres, 'tilburguni'))  ~  'UvT',
        (is.na(universiteit) & str_detect(email_adres, 'uu.nl'))  ~  'UU',
        .default = str_squish(universiteit)
      ),
      universiteit = ifelse("" == universiteit, NA_character_, universiteit)
    ) |>
    distinct(.keep_all=TRUE)
}  

source = file.path("data", "raw_data")
files = list.files(source, pattern = "scholarid.xlsx")
data = fread(files, source) |> 
  fix_scholar_names() |>
  clean_universities() |>
  fix_email_adresses() |>
  fix_google_scholar_id() |>
  select(-specialisatie, -notitie, -additional) |>
  arrange(discipline, date, naam, universiteit)

#! TODO: implement an scaper to fetch google_scholar_ids
#  - It seems like scholar package is not up to date anymore
#  - loading the url directly via rvest and httr also fails.
#  - i experimented a little in python with the scholarly package
#    In no cases does this lead to a working implementation for google_scholar_ids



parse_job_titles = function(data){
  data |>
    select(date, naam, functie) |>
    mutate(
      is_visiting = case_when(
        str_detect(str_to_lower(functie), 'gast') ~ TRUE,
        str_detect(str_to_lower(functie), 'visit') ~ TRUE,
        str_detect(str_to_lower(functie), 'external') ~ TRUE,
        str_detect(str_to_lower(functie), 'buiten') ~ TRUE,      # twijfel
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_associate_professor = case_when(
        str_detect(str_to_lower(functie), 'hoofddocent') ~ TRUE,
        str_detect(str_to_lower(functie), 'associate ') ~ TRUE,
        str_detect(str_to_lower(functie), 'uhd') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_assistant_professor = case_when(
        is_associate_professor ~ FALSE,
        str_detect(str_to_lower(functie), 'universitair docent') ~ TRUE,
        str_detect(str_to_lower(functie), 'assistant ') ~ TRUE, 
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_postdoc = case_when(
        str_detect(str_to_lower(functie), 'postdoc') ~ TRUE,
        str_detect(str_to_lower(functie), 'doctoral') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_senior = case_when(
        str_detect(str_to_lower(functie), 'senior') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_junior = case_when(
        str_detect(str_to_lower(functie), 'junior') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_lecturer = case_when(
        is_associate_professor ~ FALSE, 
        is_assistant_professor ~ FALSE, 
        str_detect(str_to_lower(functie), 'lecturer') ~ TRUE,
        str_detect(str_to_lower(functie), 'docent') ~ TRUE,
        str_detect(str_to_lower(functie), 'teacher') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_researcher = case_when(
        is_associate_professor ~ FALSE, 
        is_assistant_professor ~ FALSE,
        is_postdoc ~ FALSE, 
        str_detect(str_to_lower(functie), 'onderzoeker') ~ TRUE,
        str_detect(str_to_lower(functie), 'research') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_phd = case_when(
        str_detect(str_to_lower(functie), 'phd') ~ TRUE,
        str_detect(str_to_lower(functie), 'promovend') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_professor = case_when(
        is_associate_professor ~ FALSE, 
        is_assistant_professor ~ FALSE,
        is_postdoc ~ FALSE, 
        str_detect(str_to_lower(functie), 'hoogleraar') ~ TRUE,
        str_detect(str_to_lower(functie), 'professor') ~ TRUE,
        str_detect(str_to_lower(functie), 'proffessor') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_emeritus = case_when(
        is_professor & str_detect(str_to_lower(functie), 'emiri') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_endowed = case_when(
        is_professor & str_detect(str_to_lower(functie), 'bijzon') ~ TRUE,
        is.na(functie) ~ NA,
        .default = FALSE
      ),
      is_staff = case_when(
        # make sure that people with other positions are not falsely
        # been configured to be a staff member.
        is_associate_professor ~ FALSE, 
        is_assistant_professor ~ FALSE,
        is_lecturer ~ FALSE,
        is_postdoc ~ FALSE,
        is_professor ~ FALSE,
        # staff members have wildly varying job titles.
        str_detect(str_to_lower(functie), 'advisor') ~ TRUE,
        str_detect(str_to_lower(functie), 'secretary') ~ TRUE,
        str_detect(str_to_lower(functie), 'assistent') ~ TRUE,
        str_detect(str_to_lower(functie), 'medewerk') ~ TRUE,
        str_detect(str_to_lower(functie), 'market') ~ TRUE,
        str_detect(str_to_lower(functie), 'managing') ~ TRUE,
        str_detect(str_to_lower(functie), 'manager') ~ TRUE,
        str_detect(str_to_lower(functie), 'coordinator') ~ TRUE,
        str_detect(str_to_lower(functie), 'director') ~ TRUE,
        str_detect(str_to_lower(functie), 'directeur') ~ TRUE,
      )
    )
}



construct_positions = function(data) {
  data |>
    mutate(
      # make flags for people with one of the following distinctions
      visiting = ifelse(is_visiting, 'Visiting', NA_character_),
      senior = ifelse(is_senior, 'Senior', NA_character_),
      junior = ifelse(is_junior, 'Junior', NA_character_),
      emeritus = ifelse(is_emeritus, 'Emeritus', NA_character_),
      endowed = ifelse(is_endowed, 'Endowed', NA_character_),
      # create a basic positions variable, excluding distinctions
      position = case_when(
        is_professor ~ "Full Professor",
        is_associate_professor ~ "Associate Professor",
        is_assistant_professor ~ "Assistant Professor",
        is_postdoc ~ "Postdoctoral Researcher",
        is_phd ~ "PhD Candidate",
        is_lecturer ~ "Lecturer",
        is_researcher ~ "Researcher",
        is_staff ~ "Staff",
        .default = NA_character_
      )
    ) |>
    select(!starts_with('is_')) |>
    # combine the distinctions and positions
    unite('position2', visiting:position, na.rm=TRUE, remove=FALSE)
}

# dit is mogelijk ook een mooie extra wet. rev.


test = data |> 
  parse_job_titles() |>
  construct_positions()

data['position'] = test$position
data['position2'] = test$position2

# This is a lot of code, but the logic works to harmonize
# the various jobtitles that have been collected. 
# some points to consider:
# - where to allocate "fellow",
# 