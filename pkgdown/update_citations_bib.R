# Rebuilds vignettes/references_citations.bib from OpenAlex, replacing the
# Zotero export that used to be maintained by hand.
#
# Source the file and it does the lot. Nothing is left in the global
# environment, and the .bib is written in full, so anything added to it by hand
# is lost. The old Zotero export is kept in pkgdown/references_citations_zotero.bib.

local({

  work_id <- "W2914382202" # the respR paper on OpenAlex
  out <- "vignettes/references_citations.bib"

  # Peer reviews and datasets cite the paper but are not studies using it, and
  # respR's own CRAN record comes through as a dataset
  drop_types <- c("peer-review", "dataset")

  # Proper nouns and bare genus names that keep their capital when a Title Case
  # or ALL CAPS title is put into sentence case. Add to this as needed.
  keep_caps <- c("Antarctic", "Arctic", "Atlantic", "Pacific", "Mediterranean",
                 "African", "Japanese", "Chinook", "Dungeness", "Daphnia")
  stop_words <- c("a", "an", "and", "as", "at", "but", "by", "for", "from", "in", "into",
                  "not", "of", "on", "or", "the", "to", "via", "with")

  # --- citing works, from OpenAlex ------------------------------------------

  fields <- "id,doi,title,publication_year,type,primary_location,authorships"
  cursor <- "*"
  res <- list()
  while (!is.null(cursor)) {
    url <- sprintf(
      "https://api.openalex.org/works?filter=cites:%s&select=%s&per-page=200&cursor=%s",
      work_id, fields, utils::URLencode(cursor, reserved = TRUE)
    )
    page <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    res <- c(res, page$results)
    cursor <- page$meta$next_cursor
    if (length(page$results) == 0) cursor <- NULL
  }

  pluck <- function(w, f, empty = NA_character_) if (is.null(w[[f]])) empty else w[[f]]

  works <- data.frame(
    oa_id   = vapply(res, \(w) sub("^https://openalex.org/", "", w$id), character(1)),
    doi     = vapply(res, \(w) sub("^https://doi.org/", "", pluck(w, "doi")), character(1)),
    title   = vapply(res, pluck, character(1), "title"),
    year    = vapply(res, \(w) pluck(w, "publication_year", NA_integer_), integer(1)),
    type    = vapply(res, pluck, character(1), "type"),
    source  = vapply(res, \(w) {
      src <- w$primary_location$source$display_name
      if (is.null(src)) NA_character_ else src
    }, character(1)),
    authors = vapply(res, \(w) {
      paste(vapply(w$authorships, \(a) a$author$display_name, character(1)), collapse = " and ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  works <- works[!works$type %in% drop_types, ]
  works <- works[order(works$year, works$title), ]
  row.names(works) <- NULL

  # Citation keys in the usual surname-year-word style
  surname <- sub("^.* ", "", sub(" and .*$", "", works$authors))
  word <- vapply(strsplit(works$title, "[^A-Za-z]+"), \(w) {
    w <- w[nchar(w) > 3]
    if (length(w) == 0) "" else tolower(w[1])
  }, character(1))
  works$key <- make.unique(paste0(gsub("[^A-Za-z]", "", surname), works$year, word), sep = "_")

  # --- BibTeX ---------------------------------------------------------------

  # Publisher BibTeX from doi.org content negotiation, which is closer to a
  # Zotero export than anything OpenAlex holds. NA if there is none to be had.
  doi_bibtex <- function(doi, key) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, Accept = "application/x-bibtex; charset=utf-8")
    r <- try(curl::curl_fetch_memory(paste0("https://doi.org/", doi), handle = h), silent = TRUE)
    if (inherits(r, "try-error") || r$status_code != 200) return(NA_character_)
    txt <- rawToChar(r$content)
    Encoding(txt) <- "UTF-8"
    if (!grepl("^\\s*@", txt)) return(NA_character_)

    # Use our own key. Some agencies put commas in theirs (MDPI leaks the whole
    # title into it), so cut to the first field rather than the first comma.
    txt <- sub("(?s)^(\\s*@[A-Za-z]+\\s*\\{).*?(?=[A-Za-z]+\\s*=)",
               paste0("\\1", key, ", "), trimws(txt), perl = TRUE)
    # A few records have no title; fall back to the OpenAlex fields instead
    if (!grepl("(?i)\\btitle\\s*=", txt, perl = TRUE)) return(NA_character_)
    # An empty first author leaves "author={ and Smith, J}"
    txt <- gsub("(author\\s*=\\s*\\{)\\s*and\\s+", "\\1", txt)
    # Crossref titles carry HTML markup, which would render as literal tags
    txt <- gsub("(?i)<(i|em)>(.*?)</\\1>", "\\\\emph{\\2}", txt, perl = TRUE)
    txt <- gsub("</?[a-zA-Z][^>]*>", "", txt, perl = TRUE)
    txt
  }

  # Entry built from OpenAlex alone, for the records with no DOI (mostly theses)
  oa_bibtex <- function(w) {
    entry <- switch(w$type, dissertation = "phdthesis", book = "book", "misc")
    f <- c(
      author    = w$authors,
      title     = w$title,
      year      = w$year,
      school    = if (entry == "phdthesis") w$source else NULL,
      publisher = if (entry == "book") w$source else NULL,
      note      = if (entry == "misc") w$source else NULL,
      doi       = w$doi,
      url       = paste0("https://openalex.org/works/", w$oa_id)
    )
    f <- f[!is.na(f) & nzchar(f)]
    lines <- sprintf("  %s = {%s},", names(f), f)
    lines[length(lines)] <- sub(",$", "", lines[length(lines)])
    paste(c(sprintf("@%s{%s,", entry, w$key), lines, "}"), collapse = "\n")
  }

  # Sentence case for titles stored in Title Case or ALL CAPS, mostly theses and
  # a few journals. Titles already in sentence case are left alone, as their
  # publishers have the proper nouns right.
  sentence_case <- function(t) {
    t <- gsub("\\s+", " ", trimws(t))
    t <- gsub("\\( ", "(", gsub(" \\)", ")", t))
    # Set \emph{} aside (species names keep their case) behind placeholders
    em <- regmatches(t, gregexpr("\\\\emph\\{[^}]*\\}", t))[[1]]
    for (k in seq_along(em)) t <- sub(em[k], sprintf("\001%d\002", k), t, fixed = TRUE)

    tok_re <- "[A-Za-z][A-Za-z'\u2019\u2010-]*"
    words <- regmatches(t, gregexpr(tok_re, t))[[1]][-1]
    words <- words[nchar(words) > 3]
    all_caps <- !grepl("[a-z]", t)
    restore <- function(x) {
      for (k in seq_along(em)) x <- sub(sprintf("\001%d\002", k), em[k], x, fixed = TRUE)
      x
    }
    if (length(words) == 0 || (!all_caps && mean(grepl("^[A-Z]", words)) <= 0.6)) return(restore(t))

    m <- gregexpr(tok_re, t)
    tok <- regmatches(t, m)[[1]]
    nxt <- c(tok[-1], "")
    new <- vapply(seq_along(tok), \(k) {
      w <- tok[k]
      hit <- match(tolower(w), tolower(keep_caps))
      if (!is.na(hit)) return(keep_caps[hit])
      if (!all_caps) {
        # Acronyms and mixed case (MP, CO2, pH), judged per hyphenated part so
        # that Food-Webs or Low-Cost are not mistaken for one
        if (any(grepl("[A-Z]", substring(strsplit(w, "[-\u2010]")[[1]], 2)))) return(w)
        # Genus followed by a Latin-looking epithet, e.g. Calanus finmarchicus
        if (grepl("^[A-Z]", w) && !nxt[k] %in% stop_words &&
            grepl("^[a-z]+(us|is|a|um|ae|i|er|ii|ps|ensis|oides)$", nxt[k])) return(w)
      }
      tolower(w)
    }, character(1))
    regmatches(t, m) <- list(new)
    t <- sub("^([^A-Za-z]*)([a-z])", "\\1\\U\\2", t, perl = TRUE)
    t <- gsub(": ([a-z])", ": \\U\\1", t, perl = TRUE)
    restore(t)
  }

  # Sentence-case the title (books keep title case) and brace-protect it, as
  # pandoc otherwise sentence-cases it itself and lowercases proper nouns
  fix_title <- function(bib, is_book) {
    pat <- "(?i)(\\btitle\\s*=\\s*)(\\{(?:[^{}]++|(?2))*\\})"
    m <- regexpr(pat, bib, perl = TRUE)
    if (m == -1) return(bib)
    hit <- regmatches(bib, m)
    val <- sub(pat, "\\2", hit, perl = TRUE)
    val <- substr(val, 2, nchar(val) - 1)
    val <- if (is_book) gsub("\\s+", " ", trimws(val)) else sentence_case(val)
    regmatches(bib, m) <- paste0(sub(pat, "\\1", hit, perl = TRUE), "{{", val, "}}")
    bib
  }

  message("Fetching BibTeX for ", nrow(works), " works")
  works$bibtex <- NA_character_
  for (i in seq_len(nrow(works))) {
    if (!is.na(works$doi[i])) {
      works$bibtex[i] <- doi_bibtex(works$doi[i], works$key[i])
      Sys.sleep(0.2) # be polite to doi.org
    }
    if (is.na(works$bibtex[i])) works$bibtex[i] <- oa_bibtex(works[i, ])
    # Some agency records carry no year, which renders as "n.d."
    if (!grepl("(?i)\\byear\\s*=", works$bibtex[i], perl = TRUE) && !is.na(works$year[i]))
      works$bibtex[i] <- sub("^(\\s*@[A-Za-z]+\\s*\\{[^,]*,)",
                             sprintf("\\1 year = {%s},", works$year[i]), works$bibtex[i])
    works$bibtex[i] <- fix_title(works$bibtex[i], identical(works$type[i], "book"))
    cat(".")
  }
  cat("\n")

  header <- c(
    sprintf("%% Generated by pkgdown/update_citations_bib.R on %s", Sys.Date()),
    sprintf("%% Works citing OpenAlex %s, %d entries", work_id, nrow(works)),
    ""
  )
  writeLines(c(header, paste(works$bibtex, collapse = "\n\n")), out)
  message("Wrote ", nrow(works), " entries to ", out)
})
