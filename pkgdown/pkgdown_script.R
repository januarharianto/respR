## Build site

pkgdown::build_site()

pkgdown::build_site(preview = TRUE)

# do it in the background
job::job({pkgdown::build_site(preview = TRUE)})

## Quicker
## Only builds what has changed
## Build pkg first
pkgdown::build_site(lazy = TRUE, devel = TRUE, preview = TRUE)

# ? not sure what this does
#pkgdown::preview_site(pkg = ".", path = ".", preview = NA)

pkgdown::build_home(preview = TRUE)

pkgdown::build_article("closed")
pkgdown::build_article("subset_rate")

usethis::use_vignette("support", "Support Us")


# sets up GH Actions workflow. Which isn't working.
#usethis::use_pkgdown_github_pages()


## This deploys to the gh-pages branch, and runs the deploy workflow on GH Actions
## Uses local changes - don't have to be pushed - IS THIS RIGHT?
## (not sure about commits v local changes tho)
pkgdown::deploy_to_branch()
