options(renv.config.auto.restore = TRUE)
if (file.exists("renv/activate.R")) source("renv/activate.R")

library(tidyverse)
library(stringr)

# Rename helper only renames files that when it needs to be renamed
.safe_rename <- function(src_files, dst_files) {
  idx = src_files != dst_files
  if (sum(idx) == 0 ) return(invisible(NULL))
  
  for (i in sum(idx)){
    ok = file.rename(src_files[idx][i], dst_files[idx][i])
    if (any(!ok)) warning(
      "Some renames failed: ",
      paste(basename(src_files[i][!ok]), collapse = ", ")
    )
  }
}

# Enforces that all the files with key in the subdirectory are renamed
# such that any but the last files have the "old_" prefix
enforce_latest_name = function(sub, key){
  # set the target dir
  target_dir = file.path('data', sub)

  # match both unprefixed and old_ prefixed files like:
  # 20251027oaworks.Rds  and  old_20251025oaworks.Rds
  pat_all = paste0("^(?:old_)?\\d{8}.*", key, "\\.Rds$")
  files = list.files(target_dir, pattern = pat_all)
  k = length(files)
  if (!length(files)) return(invisible(NULL))

  # sort by date (and remove "old_" to sort basenames consistently)
  f = tibble(
    src = files,
    old = ifelse(str_detect(files, 'old_'), 'old', NA_character_),
    base = files |> stringr::str_replace('^old_', '')
  ) |>
  arrange(base) |>
  mutate(
    dst = paste0(if_else(row_number() < n(), "old_", ""), base)
  )

  # add prefix to all but the last file
  dst_files = file.path(target_dir, f |> pull(dst))
  src_files = file.path(target_dir, f |> pull(src))

  # rename the files
  .safe_rename(src_files, dst_files)
}

# Drive it for your folders/keys
update_latest = function() {
  keys_per_sub = list(
    raw_data = c("oascholars", "oaworks", "oaauthors"),
    processed = c("ethnicity", "gender", "names", "scholarid", "scholars")
  )

  # enforces the latest name for all keys_per_sub
  for (sub in names(keys_per_sub)){
    keys = keys_per_sub[[sub]]
    for (key in keys) enforce_latest_name(sub, key)
  }
}

update_latest()
