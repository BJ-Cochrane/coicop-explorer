# Lookups, search and small HTML helpers.

version_name  <- function(v) VERSIONS$name[match(v, VERSIONS$id)]
version_short <- function(v) VERSIONS$short[match(v, VERSIONS$id)]
version_color <- function(v) VERSIONS$color[match(v, VERSIONS$id)]

get_code <- function(v, code) {
  CODES[CODES$version == v & CODES$code == code, , drop = FALSE]
}
children_of <- function(v, code) {
  CODES[CODES$version == v & !is.na(CODES$parent) & CODES$parent == code, , drop = FALSE]
}
code_exists <- function(v, code) any(CODES$version == v & CODES$code == code)

# "07.1.1.1" -> c("07", "07.1", "07.1.1")
ancestors_of <- function(code) {
  parts <- strsplit(code, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(character())
  vapply(seq_len(length(parts) - 1), function(k) paste(parts[1:k], collapse = "."), "")
}

# ECOICOP ver.1 has no notes, so use the matching COICOP 1999 class
notes_for <- function(row) {
  if (row$version == "ecoicop1" && !is.na(row$note_ref)) {
    src <- get_code("coicop1999", row$note_ref)
    if (nrow(src)) return(list(row = src, inherited = TRUE, from = row$note_ref))
  }
  list(row = row, inherited = FALSE, from = NULL)
}

# Search ----

FIELDS <- c(title = "Title", intro = "Description", includes = "Includes",
            also_includes = "Also includes", excludes = "Excludes")
FIELD_WEIGHT <- c(title = 10, intro = 3, includes = 4, also_includes = 3, excludes = 1)

re_lit <- function(x) paste0("\\Q", gsub("\\E", "\\E\\\\E\\Q", x, fixed = TRUE), "\\E")

# "0711" -> "07.1.1"
cp_dot <- function(digits) {
  ch <- strsplit(digits, "")[[1]]
  if (length(ch) <= 2) return(digits)
  paste(c(paste(ch[1:2], collapse = ""), ch[-(1:2)]), collapse = ".")
}

search_codes <- function(query, versions, levels = NULL, types = NULL, in_notes = TRUE) {
  d <- CODES[CODES$version %in% versions, , drop = FALSE]
  if (length(levels)) d <- d[d$level %in% as.integer(levels), , drop = FALSE]
  if (length(types))  d <- d[!is.na(d$type) & d$type %in% types, , drop = FALSE]
  q <- tolower(trimws(gsub("\\s+", " ", query)))

  if (!nzchar(q)) {
    d$score <- 0; d$hit <- NA_character_; d$snippet <- NA_character_
    d <- d[d$level <= 2, , drop = FALSE]
    return(d[order(match(d$version, versions), d$code), , drop = FALSE])
  }

  # codes: "07.1", "0711", "CP0711"
  qc <- sub("^cp", "", gsub("\\s", "", q))
  if (grepl("^[0-9]{2}([.]?[0-9])*$", qc)) {
    dotted <- if (grepl(".", qc, fixed = TRUE)) qc else cp_dot(qc)
    out <- d[startsWith(d$code, dotted), , drop = FALSE]
    out$score <- 0; out$hit <- "Code"; out$snippet <- NA_character_
    return(out[order(match(out$version, versions), out$code), , drop = FALSE])
  }

  terms <- strsplit(q, " ", fixed = TRUE)[[1]]
  fields <- if (in_notes) names(FIELDS) else "title"
  hay <- if (in_notes) d$search else tolower(paste(d$code, d$title))
  d <- d[Reduce(`&`, lapply(terms, \(t) grepl(t, hay, fixed = TRUE))), , drop = FALSE]
  if (!nrow(d)) { d$score <- numeric(); d$hit <- character(); d$snippet <- character(); return(d) }

  # Score each field. Word starts count double, and in titles a whole word
  # (or its plural) gets a further bonus so "pet" finds "Pets" before "Petrol".
  count <- function(txt, pattern) Reduce(`+`, lapply(terms, \(t) grepl(pattern(t), txt, perl = TRUE)))
  score <- numeric(nrow(d)); best <- rep(NA_character_, nrow(d)); best_w <- rep(-1, nrow(d))
  for (f in fields) {
    txt <- tolower(d[[f]]); txt[is.na(txt)] <- ""
    n  <- count(txt, re_lit)
    wb <- count(txt, \(t) paste0("\\b", re_lit(t)))
    w  <- FIELD_WEIGHT[[f]] * (n + wb)
    if (f == "title") w <- w + 5 * wb + 6 * count(txt, \(t) paste0("\\b", re_lit(t), "(s|es)?\\b"))
    score <- score + w
    better <- w > best_w & n > 0
    best[better] <- f; best_w[better] <- w[better]
  }
  d$score <- score - 0.15 * d$level
  d$hit <- unname(FIELDS[best])
  d$snippet <- vapply(seq_len(nrow(d)), function(i) {
    if (is.na(best[i]) || best[i] == "title") return(NA_character_)
    make_snippet(d[[best[i]]][i], terms)
  }, "")
  d[order(-round(d$score, 2), match(d$version, versions), d$code), , drop = FALSE]
}

make_snippet <- function(text, terms, width = 70) {
  if (is.na(text)) return(NA_character_)
  text <- gsub("\n", " · ", text, fixed = TRUE)
  pos <- regexpr(terms[1], tolower(text), fixed = TRUE)
  if (pos < 0) pos <- 1
  start <- max(1, pos - width); end <- min(nchar(text), pos + width)
  s <- htmlEscape(substr(text, start, end))
  for (t in unique(terms)) {
    s <- gsub(paste0("(", re_lit(htmlEscape(t)), ")"), "<mark>\\1</mark>", s, ignore.case = TRUE, perl = TRUE)
  }
  paste0(if (start > 1) "…", s, if (end < nchar(text)) "…")
}

# HTML ----

version_tag <- function(v) {
  span(class = "vtag", style = sprintf("--vc:%s", version_color(v)), version_short(v))
}

type_badge <- function(type) {
  if (is.na(type) || type == "Mixed") return(NULL)
  span(class = paste0("tbadge t-", type), title = META$type_labels[[type]], type)
}

code_link <- function(v, code, label = code) {
  plain <- !(is.character(label) && identical(label, code))
  tags$a(href = "#", class = paste("code-link", if (plain) "plain"), `data-v` = v, `data-code` = code,
         label, .noWS = "outside")
}

# Turn references like "(01.1.1.2)" into links
linkify_codes <- function(text_html, version) {
  m <- gregexpr("\\b[0-9]{1,2}(\\.[0-9]){1,4}\\b", text_html, perl = TRUE)
  regmatches(text_html, m) <- lapply(regmatches(text_html, m), function(found) {
    vapply(found, function(cd) {
      parts <- strsplit(cd, ".", fixed = TRUE)[[1]]
      parts[1] <- sprintf("%02d", as.integer(parts[1]))  # the UN file has "(1.1.1.9)" in one place
      target <- paste(parts, collapse = ".")
      if (!code_exists(version, target)) return(cd)
      sprintf('<a href="#" class="code-link" data-v="%s" data-code="%s">%s</a>', version, target, cd)
    }, "", USE.NAMES = FALSE)
  })
  text_html
}

note_list <- function(text, version) {
  if (is.na(text) || !nzchar(text)) return(NULL)
  items <- strsplit(text, "\n", fixed = TRUE)[[1]]
  tags$ul(class = "note-list", lapply(items, \(i) tags$li(HTML(linkify_codes(htmlEscape(i), version)))))
}

short_text <- function(x, n = 34) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "…"), x)

empty_state <- function(title, body = NULL) {
  div(class = "empty", p(class = "empty-title", title), if (!is.null(body)) p(body))
}
