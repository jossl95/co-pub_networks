# this utility initializes the renv for this project
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Start with a clean, empty project library
if (!file.exists("renv/activate.R")) renv::init(bare = TRUE)

# Snapshot only detected dependencies (library/require/requireNamespace/pkg::fun)
renv::settings$snapshot.type("implicit")
renv::settings$use.cache(TRUE)

# Scan ONLY your content dir
deps <- renv::dependencies(path = "src/docs/content", progress = FALSE)
pkgs <- unique(c(na.omit(deps$Package), 'knitr', 'rmarkdown'))

# ---- map non-CRAN packages to their remote specs ----
remote_map <- c(
  DemografixeR = "matbmeijer/DemografixeR", # <- GitHub-only package
  sqids = "sqids/sqids-r",
  genderizeR = "kalimu/genderizeR"
)

# replace package names with remote specs where applicable
specs <- vapply(
  pkgs, function(p) 
    if (p %in% names(remote_map)) remote_map[[p]] 
    else p, character(1)
)

# Pre-install into project library, and lock exact versions (GitHub SHAs included)
if (length(specs)) renv::install(specs)
renv::snapshot(prompt = FALSE)

# Tip: to avoid GitHub rate limits when installing remotes, set GITHUB_PAT in your .Renviron
# (e.g., GITHUB_PAT=ghp_xxx...)