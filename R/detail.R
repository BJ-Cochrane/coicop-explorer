# Detail panel for one category: header, notes, children, correspondence, HICP.

strip_type <- function(title) sub("\\s*\\((ND|SD|D|S)\\)\\s*$", "", title)

stat_tile <- function(label, value, sub = NULL, color = NULL) {
  div(class = "stat", style = if (!is.null(color)) sprintf("--vc:%s", color),
      div(class = "stat-label", label),
      div(class = "stat-value", value),
      if (!is.null(sub)) div(class = "stat-sub", sub))
}

detail_header <- function(row) {
  anc <- ancestors_of(row$code)
  crumbs <- lapply(anc, function(a) {
    tagList(code_link(row$version, a, paste(a, get_code(row$version, a)$title)), span(class = "sep", "/"))
  })
  meta <- list(
    row$level_name,
    if (!is.na(row$type)) {
      if (row$type == "Mixed") "Mixed product types"
      else tagList(type_badge(row$type), META$type_labels[[row$type]])
    },
    if (!is.na(row$eurostat_code)) span(class = "mono", title = "Eurostat code", row$eurostat_code),
    if (row$version == "coicop2018" && row$level == 5) "Optional FAO food detail"
  )
  meta <- Filter(Negate(is.null), meta)
  tagList(
    div(class = "crumbs", version_tag(row$version), crumbs),
    div(class = "detail-heading",
        span(class = "code-big", row$code),
        h2(class = "detail-title", strip_type(row$title))),
    div(class = "detail-meta", lapply(meta, \(m) span(class = "meta", m)))
  )
}

detail_notes <- function(row) {
  nt <- notes_for(row)
  n <- nt$row
  v_link <- if (nt$inherited) "coicop1999" else row$version
  has <- !all(is.na(c(n$intro, n$includes, n$also_includes, n$excludes)))
  block <- function(cls, heading, text, lede = NULL) {
    if (is.na(text)) return(NULL)
    div(class = paste("notes", cls), h3(heading), if (!is.null(lede)) p(class = "lede", lede),
        note_list(text, v_link))
  }
  tagList(
    if (nt$inherited) div(class = "aside",
      "ECOICOP ver.1 has no notes of its own. These are the notes for COICOP 1999 ",
      code_link("coicop1999", nt$from, paste("class", nt$from)), "."),
    if (!has) empty_state("No explanatory notes",
      if (row$level <= 2) "Notes for this category are given at the levels below it."
      else "The title is the full definition."),
    if (!is.na(n$intro)) div(class = "intro",
      lapply(strsplit(n$intro, "\n")[[1]], \(x) p(HTML(linkify_codes(htmlEscape(x), v_link))))),
    block("inc", "Includes", n$includes),
    block("also", "Also includes", n$also_includes),
    block("exc", "Excludes", n$excludes, "Classified elsewhere. Follow the code to see where.")
  )
}

detail_children <- function(row) {
  kids <- children_of(row$version, row$code)
  up <- if (!is.na(row$parent)) {
    p <- get_code(row$version, row$parent)
    div(class = "part-of", "Part of ", code_link(row$version, p$code, paste(p$code, p$title)))
  }
  if (!nrow(kids)) return(tagList(up, empty_state("Lowest level", "This category is not divided further.")))
  tagList(up, div(class = "child-list", lapply(seq_len(nrow(kids)), function(i) {
    k <- kids[i, ]
    tags$a(href = "#", class = "child code-link", `data-v` = k$version, `data-code` = k$code,
           span(class = "child-code", k$code),
           span(class = "child-title", strip_type(k$title)),
           type_badge(k$type),
           span(class = "child-n", if (k$n_children > 0) k$n_children))
  })))
}

corr_item <- function(v, code, title, rel = NULL, note = NULL, count = NULL) {
  div(class = "corr-item",
      version_tag(v),
      code_link(v, code, span(class = "mono", code)),
      span(class = "corr-title", title),
      if (!is.null(count)) span(class = "corr-count", count),
      if (!is.null(rel) && !is.na(rel)) span(class = paste("rel", if (grepl("^One", rel)) "rel-1" else "rel-n"), rel),
      if (!is.null(note) && !is.na(note)) div(class = "corr-note", "Common content: ", note))
}

detail_corr <- function(row) {
  v <- row$version; code <- row$code
  under <- function(x, cd) x == cd | startsWith(x, paste0(cd, "."))
  out <- list()
  if (v == "coicop2018") {
    exact <- CORR[CORR$code2018 == code, ]
    if (nrow(exact)) {
      out <- c(out, list(h3("In COICOP 1999"),
        lapply(seq_len(nrow(exact)), \(i) corr_item("coicop1999", exact$code1999[i], exact$title1999[i],
                                                     exact$relation[i], exact$note[i]))))
    } else if (row$level == 5) {
      p <- CORR[CORR$code2018 == row$parent, ]
      out <- c(out, list(
        div(class = "aside", "The FAO food detail is not in the correspondence table. These are the links for its subclass ",
            code_link(v, row$parent), "."),
        lapply(seq_len(nrow(p)), \(i) corr_item("coicop1999", p$code1999[i], p$title1999[i], p$relation[i], p$note[i]))))
    } else {
      sub <- CORR[under(CORR$code2018, code) & CORR$level1999 == 3, ]
      if (nrow(sub)) {
        agg <- aggregate(list(n = sub$code2018), by = list(code1999 = sub$code1999, title1999 = sub$title1999),
                         FUN = \(x) length(unique(x)))
        agg <- agg[order(-agg$n, agg$code1999), ]
        out <- c(out, list(h3("COICOP 1999 classes its subclasses come from"),
          lapply(seq_len(nrow(agg)), \(i) corr_item("coicop1999", agg$code1999[i], agg$title1999[i],
            count = sprintf("%d subclass%s", agg$n[i], if (agg$n[i] > 1) "es" else "")))))
      }
    }
  } else {
    c99 <- if (v == "ecoicop1") row$note_ref else code
    if (v == "ecoicop1") out <- c(out, list(div(class = "aside",
      "ECOICOP ver.1 is built on COICOP 1999 (", if (c99 == code) "same category " else "parent class ",
      code_link("coicop1999", c99), "), so the links to COICOP 2018 go through it.")))
    sub <- CORR[under(CORR$code1999, c99) & CORR$level2018 == 4, ]
    if (nrow(sub)) {
      sub <- sub[order(sub$code2018), ]
      out <- c(out, list(h3(sprintf("COICOP 2018 subclasses drawn from 1999 %s", c99)),
        lapply(seq_len(nrow(sub)), \(i) corr_item("coicop2018", sub$code2018[i], sub$title2018[i],
                                                   sub$relation[i], sub$note[i]))))
    }
  }
  same <- CODES[CODES$code == code & CODES$version != v, ]
  if (nrow(same)) out <- c(out, list(
    h3("Same code number elsewhere"),
    p(class = "lede", "The number matches. The content may not."),
    lapply(seq_len(nrow(same)), \(i) corr_item(same$version[i], same$code[i], same$title[i]))))
  if (!length(out)) return(empty_state("No correspondence recorded"))
  tagList(out)
}

detail_hicp <- function(row) {
  v <- row$version
  if (v == "coicop1999") {
    alt <- if (code_exists("ecoicop1", row$code)) row$code
    return(empty_state("No price data for COICOP 1999",
      tagList("The HICP used ECOICOP ver.1, which is built on COICOP 1999.",
              if (!is.null(alt)) tagList(" See ", code_link("ecoicop1", alt, paste("ECOICOP ver.1", alt)), "."))))
  }
  lw <- LATEST_W[[v]]; li <- LATEST_I[[v]]
  w_eu <- WEIGHTS$value[WEIGHTS$version == v & WEIGHTS$code == row$code & WEIGHTS$geo == "EU27_2020" & WEIGHTS$year == lw]
  i_eu <- INFL$value[INFL$version == v & INFL$code == row$code & INFL$geo == "EU27_2020" & INFL$year == li]
  m <- MONTHLY[MONTHLY$version == v & MONTHLY$code == row$code & MONTHLY$geo == "EU27_2020", ]
  m <- m[order(m$month), ]
  if (!length(w_eu) && !length(i_eu) && !nrow(m))
    return(empty_state("Not published in the HICP",
      "The HICP only covers household spending, and Eurostat does not publish every category separately."))
  pct <- function(x, sign = FALSE) sprintf(if (sign) "%+.1f%%" else if (x < 1) "%.2f%%" else "%.1f%%", x)
  tagList(
    div(class = "stats",
      stat_tile(paste("Share of EU basket,", lw), if (length(w_eu)) pct(w_eu / 10) else "–",
                if (length(w_eu)) sprintf("%.2f‰ of HICP weight", w_eu)),
      stat_tile(paste("EU inflation,", li), if (length(i_eu)) pct(i_eu, TRUE) else "–", "annual average"),
      stat_tile("EU, latest month", if (nrow(m)) pct(tail(m$value, 1), TRUE) else "–",
                if (nrow(m)) paste(format(tail(m$month, 1), "%B %Y"), "on a year earlier") else "COICOP 2018 only")),
    h3("Inflation against all items, EU"),
    plotlyOutput("d_infl", height = "240px"),
    if (v == "coicop2018") tagList(h3("Monthly, year on year"), plotlyOutput("d_month", height = "220px")),
    h3("Weight in the basket",
       if (v == "coicop2018") span(class = "h-note", "shaded years are Eurostat back-calculations")),
    plotlyOutput("d_wts", height = "240px"),
    h3(paste("Weight by country,", lw)),
    plotlyOutput("d_country", height = "640px")
  )
}
