pacman::p_load(here)
here()
rmarkdown::render_site(here("site"),
                       output_format = "bookdown::gitbook",
                       encoding = "UTF-8")
old_path <- here("site", "docs")
new_dir <- here("docs")
suppressWarnings(unlink(new_dir, recursive = TRUE))
file.rename(old_path, new_dir)
