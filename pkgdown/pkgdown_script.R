## Notes and commands
##
## For now - we keep README.md updated in here, and copy to README.md in respR directory.
## Probably a better way of keeping them in sync, but for now do this.

pkgdown::build_site()

pkgdown::build_site(lazy = TRUE, devel = TRUE)

pkgdown::preview_site(pkg = ".", path = ".", preview = NA)


pkgdown::build_site(preview = TRUE)

pkgdown::build_home(preview = TRUE)

pkgdown::build_article("closed")
pkgdown::build_article("subset_rate")
