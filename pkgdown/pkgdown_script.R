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

# builds top page
pkgdown::build_home(preview = TRUE)

# build a single article
pkgdown::build_article("closed")

pkgdown::build_article("flowthrough")
pkgdown::preview_page('articles/flowthrough.html')

# add a new vignetter
usethis::use_vignette("intermittent_old", "Intermittent-flow respirometry: Alternative approaches")


# sets up GH Actions workflow. Needs tested.
#usethis::use_pkgdown_github_pages()


## This deploys to the gh-pages branch, and runs the deploy workflow on GH Actions
## Uses local changes - don't have to be pushed - IS THIS RIGHT?
## (not sure about commits v local changes tho)
## Sometimes gets stuck on git command.
## Stopping and building site then trying again seems to work.
## Although last time took MANY attempts
##
## BUT - even if it seems to freeze - check GHub and site - it might have successfully
## triggered the 'pages build and deployment' GH action anyway...
pkgdown::deploy_to_branch()
