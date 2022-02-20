## Build site

pkgdown::build_site()

pkgdown::build_site(preview = TRUE)

# do it in the background
job::job({pkgdown::build_site(preview = TRUE)})

## Quicker
## Only builds what has changed
pkgdown::build_site(lazy = TRUE, devel = TRUE)

# ? not sure what this does
#pkgdown::preview_site(pkg = ".", path = ".", preview = NA)

pkgdown::build_home(preview = TRUE)

pkgdown::build_article("closed")
pkgdown::build_article("subset_rate")

usethis::use_vignette("support", "Support Us")
