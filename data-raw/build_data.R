# Builds data/coicop.rds from the files in data-raw/. Needs readxl and data.table.
# Rscript data-raw/build_data.R  (from the project folder)

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
})

raw <- function(f) file.path("data-raw", f)
corr_xlsx <- raw("COICOP2018_COICOP1999_correspondence_table_final.xlsx")

# Helpers ----
clean_txt <- function(x) {
  x <- gsub("\r+\n|\r", "\n", x)
  x <- gsub(" ", " ", x)
  x <- trimws(x)
  x[x %in% c("", "-")] <- NA
  x
}
# "* a\n* b" -> "a\nb"
bullets_2018 <- function(x) {
  x <- clean_txt(x)
  out <- vapply(strsplit(ifelse(is.na(x), "", x), "\n", fixed = TRUE), function(l) {
    l <- trimws(sub("^\\*\\s*", "", trimws(l)))
    paste(l[nzchar(l)], collapse = "\n")
  }, "")
  out[!nzchar(out)] <- NA
  out
}
# Titles end in (ND), (SD), (D) or (S)
type_of <- function(title) {
  m <- regmatches(title, regexpr("\\((ND|SD|D|S)\\)\\s*$", title))
  out <- rep(NA_character_, length(title))
  out[grepl("\\((ND|SD|D|S)\\)\\s*$", title)] <- gsub("[() ]", "", m)
  out
}
parent_of <- function(code) {
  p <- sub("\\.[^.]*$", "", code)
  p[!grepl(".", code, fixed = TRUE)] <- NA
  p
}
# Eurostat "CP01111" -> "01.1.1.1"
cp_to_dot <- function(id) {
  d <- sub("^CP", "", id)
  vapply(d, function(s) {
    ch <- strsplit(s, "")[[1]]
    if (length(ch) <= 2) return(s)
    paste(c(paste(ch[1:2], collapse = ""), ch[-(1:2)]), collapse = ".")
  }, "", USE.NAMES = FALSE)
}
dot_to_cp <- function(code) paste0("CP", gsub(".", "", code, fixed = TRUE))

# COICOP 2018 (UN) ----
c18 <- as.data.table(read_excel(raw("COICOP_2018_English_structure.xlsx")))
c18[, `:=`(
  version       = "coicop2018",
  title         = trimws(title),
  intro         = clean_txt(intro),
  includes      = bullets_2018(includes),
  also_includes = bullets_2018(alsoIncludes),
  excludes      = bullets_2018(excludes)
)]
c18[, level := (nchar(code) + 1) %/% 2]
stopifnot(identical(as.vector(table(c18$level)), c(15L, 63L, 186L, 338L, 269L)))
c18 <- c18[, .(version, code, title, level, intro, includes, also_includes, excludes)]

# COICOP 1999 (UN) ----
# The notes are only in the correspondence workbook.
c99 <- as.data.table(read_excel(corr_xlsx, sheet = "COICOP 1999"))
setnames(c99, c("code", "title", "note"))
c99 <- c99[code != "01-12"]
c99[, note := clean_txt(note)]

# The 1999 notes are running text: "- item;" lines, then optional
# "Includes: a;  b." and "Excludes: c (01.2.3);  d." paragraphs.
split_items <- function(s) {
  s <- trimws(sub("[.;]\\s*$", "", s))
  it <- trimws(strsplit(s, ";\\s{1,}")[[1]])
  it[nzchar(it)]
}
parse_1999 <- function(note) {
  res <- list(intro = NA_character_, includes = NA_character_,
              also_includes = NA_character_, excludes = NA_character_)
  if (is.na(note)) return(res)
  lines <- trimws(strsplit(note, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  inc <- exc <- dash <- prose <- character()
  for (l in lines) {
    if (grepl("^Includes?:", l)) inc <- c(inc, split_items(sub("^Includes?:\\s*", "", l)))
    else if (grepl("^Excludes?:", l)) exc <- c(exc, split_items(sub("^Excludes?:\\s*", "", l)))
    else if (grepl("^-\\s*", l)) dash <- c(dash, split_items(sub("^-\\s*", "", l)))
    else prose <- c(prose, l)
  }
  inc <- sub("^and\\s+", "", inc); exc <- sub("^and\\s+", "", exc); dash <- sub("^and\\s+", "", dash)
  # group notes can end with "The group excludes: x;  y."
  if (length(prose)) {
    p <- paste(prose, collapse = "\n")
    m <- regexpr("(The (group|division|class) )?[Ee]xcludes:\\s*", p)
    if (m > 0) {
      exc <- c(exc, sub("^and\\s+", "", split_items(substring(p, m + attr(m, "match.length")))))
      p <- trimws(substr(p, 1, m - 1))
    }
    if (nzchar(p)) res$intro <- p
  }
  cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  if (length(dash)) res$includes      <- paste(cap1(dash), collapse = "\n")
  if (length(inc))  res$also_includes <- paste(cap1(inc), collapse = "\n")
  if (length(exc))  res$excludes      <- paste(cap1(exc), collapse = "\n")
  res
}
p99 <- rbindlist(lapply(c99$note, parse_1999))
c99 <- cbind(c99[, .(version = "coicop1999", code, title = trimws(title))], p99)
c99[, level := (nchar(code) + 1) %/% 2]

# ECOICOP ver.1 (Eurostat) ----
read_cl <- function(f) {
  x <- fread(raw(f), header = FALSE, sep = "\t", quote = "", col.names = c("id", "label"),
             encoding = "UTF-8")
  x[grepl("^CP[0-9]{2,}$", id) & id != "CP00"]
}
e1 <- read_cl("eurostat_COICOP_codelist.tsv")
e1 <- e1[, .(version = "ecoicop1", code = cp_to_dot(id), title = trimws(label))]
e1[, level := (nchar(code) + 1) %/% 2]
e1[, `:=`(intro = NA_character_, includes = NA_character_,
          also_includes = NA_character_, excludes = NA_character_)]

# ECOICOP ver.2 has the same codes as COICOP 2018, so check that rather than store it twice
e2 <- read_cl("eurostat_COICOP18_codelist.tsv")
e2[, code := cp_to_dot(id)]
stopifnot(setequal(e2$code, c18$code))
e2[, un_title := c18$title[match(code, c18$code)]]
# Two Eurostat titles under 01.1.8.9 are off by one row (as of Oct 2026)
e2_title_diffs <- e2[label != un_title, .(code, eurostat = label, un = un_title)]
stopifnot(nrow(e2_title_diffs) <= 5)

# Combine ----
codes <- rbindlist(list(c18, c99, e1), use.names = TRUE, fill = TRUE)
level_names <- list(
  coicop2018 = c("Division", "Group", "Class", "Subclass", "Food detail (6-digit)"),
  coicop1999 = c("Division", "Group", "Class"),
  ecoicop1   = c("Division", "Group", "Class", "Subclass")
)
codes[, level_name := level_names[[version]][level], by = version]
codes[, parent := parent_of(code)]
codes[, division := substr(code, 1, 2)]
codes[, type := type_of(title)]
codes[, eurostat_code := ifelse(version == "coicop1999", NA_character_, dot_to_cp(code))]
codes[, n_children := vapply(seq_len(.N), function(i) sum(parent == code[i], na.rm = TRUE), 0L),
      by = version]

# 6-digit food detail has no type in its title, so use the subclass's
t18 <- codes[version == "coicop2018", setNames(type, code)]
codes[version == "coicop2018" & level == 5, type := unname(t18[parent])]
# Higher levels take their type from the leaves below, or "Mixed"
codes[, leaf := n_children == 0]
for (v in unique(codes$version)) {
  vv <- codes[version == v]
  for (i in which(is.na(vv$type))) {
    kids <- vv[leaf & startsWith(code, paste0(vv$code[i], ".")) & !is.na(type), unique(type)]
    if (length(kids) > 1) codes[version == v & code == vv$code[i], type := "Mixed"]
    if (length(kids) == 1) codes[version == v & code == vv$code[i], type := kids]
  }
}

# ECOICOP ver.1 borrows notes from the nearest COICOP 1999 code above it
codes[version == "ecoicop1", note_ref := {
  have <- codes[version == "coicop1999", code]
  vapply(code, function(cd) {
    while (!is.na(cd) && !(cd %in% have)) cd <- parent_of(cd)
    cd
  }, "")
}]

# Same for its product types
t99 <- codes[version == "coicop1999", setNames(type, code)]
codes[version == "ecoicop1", type := unname(t99[note_ref])]

codes[, search := tolower(paste(code, title, intro, includes, also_includes, excludes, sep = " • "))]
codes[, search := gsub("\\bna\\b", "", search)]
codes[, leaf := NULL]
setorder(codes, version, code)

# Correspondence 2018 to 1999 ----
cr <- as.data.table(read_excel(corr_xlsx, sheet = "Correspondence 2018-1999"))
setnames(cr, c("code2018", "title2018", "code1999", "title1999", "note"))
cr[, (names(cr)) := lapply(.SD, clean_txt)]
cr[, note := sub("^common content:\\s*", "", note)]
cr <- unique(cr[!is.na(code1999)])
cr[, level2018 := (nchar(code2018) + 1) %/% 2]
cr[, level1999 := (nchar(code1999) + 1) %/% 2]
stopifnot(all(cr$code1999 %in% c99$code), all(cr$code2018 %in% c18$code))

# Classify each link, comparing like levels only
cr[, n_to := uniqueN(code1999), by = .(code2018, level1999)]
cr[, n_from := uniqueN(code2018), by = .(code1999, level2018)]
cr[, relation := fifelse(n_to == 1 & n_from == 1, "One-to-one",
                 fifelse(n_to > 1 & n_from == 1, "Split (one 2018 → many 1999)",
                 fifelse(n_to == 1 & n_from > 1, "Merge (many 2018 → one 1999)",
                         "Many-to-many")))]
cr[, c("n_to", "n_from") := NULL]

# Eurostat HICP ----
read_estat <- function(f) {
  x <- fread(raw(f), sep = "\t", header = TRUE, colClasses = "character",
             showProgress = FALSE)
  key <- names(x)[1]
  dims <- strsplit(sub("\\\\TIME_PERIOD$", "", key), ",")[[1]]
  x[, (dims) := tstrsplit(get(key), ",", fixed = TRUE)]
  x[, (key) := NULL]
  long <- melt(x, id.vars = dims, variable.name = "time", value.name = "raw",
               variable.factor = FALSE)
  long[, time := trimws(time)]
  long[, value := suppressWarnings(as.numeric(sub("\\s.*$", "", trimws(raw))))]
  long[!is.na(value)][, raw := NULL][]
}
hicp_codes <- function(d, col, version) {
  d <- d[grepl("^CP[0-9]{2,}$", get(col)) & get(col) != "CP00"]
  d[, code := cp_to_dot(get(col))]
  d[, version := version]
  d
}
message("Reading Eurostat tables...")
w2 <- hicp_codes(read_estat("prc_hicp_iw.tsv.gz"),  "coicop18", "coicop2018")
w1 <- hicp_codes(read_estat("prc_hicp_inw.tsv.gz"), "coicop",   "ecoicop1")
weights <- rbind(w2[, .(version, code, geo, year = as.integer(time), value)],
                 w1[, .(version, code, geo, year = as.integer(time), value)])
# ver.2 weights before 2026 are back-casts (flagged in the app)
i2 <- read_estat("prc_hicp_ainr.tsv.gz")[unit == "RCH_A_AVG"]
i1 <- read_estat("prc_hicp_aind.tsv.gz")[unit == "RCH_A_AVG"]
infl <- rbind(hicp_codes(i2, "coicop18", "coicop2018")[, .(version, code, geo, year = as.integer(time), value)],
              hicp_codes(i1, "coicop",   "ecoicop1")[,   .(version, code, geo, year = as.integer(time), value)])
m2 <- hicp_codes(read_estat("prc_hicp_minr_EU_EA.tsv.gz")[unit == "RCH_A"], "coicop18", "coicop2018")
monthly <- m2[, .(version, code, geo, month = as.Date(paste0(time, "-01")), value)]

# All-items rates
infl_all <- rbind(
  read_estat("prc_hicp_ainr.tsv.gz")[unit == "RCH_A_AVG" & coicop18 == "TOTAL",
                                     .(version = "coicop2018", geo, year = as.integer(time), value)],
  read_estat("prc_hicp_aind.tsv.gz")[unit == "RCH_A_AVG" & coicop == "CP00",
                                     .(version = "ecoicop1", geo, year = as.integer(time), value)])

geo_cl <- fread(raw("eurostat_GEO_codelist.tsv"), header = FALSE, sep = "\t", quote = "",
                col.names = c("geo", "label"), encoding = "UTF-8")
# EU, euro area (changing composition) and single countries only
drop_geo <- c("EU", "EU28", "EEA", "EA19", "EA20", "EA21")
geos <- geo_cl[geo %in% unique(c(weights$geo, infl$geo)) & !geo %in% drop_geo]
geos[geo == "EU27_2020", label := "European Union (27)"]
geos[geo == "EA", label := "Euro area"]
geos[, aggregate := geo %in% c("EU27_2020", "EA")]
setorder(geos, -aggregate, label)
weights  <- weights[geo %in% geos$geo]
infl     <- infl[geo %in% geos$geo]
infl_all <- infl_all[geo %in% geos$geo]

# Version metadata ----
versions <- data.table(
  id = c("coicop2018", "coicop1999", "ecoicop1"),
  name = c("COICOP 2018", "COICOP 1999", "ECOICOP ver.1"),
  short = c("2018", "1999", "ECOICOP 1"),
  publisher = c("UN Statistics Division", "UN Statistics Division", "Eurostat"),
  adopted = c("UN Statistical Commission, March 2018 (final edited structure 2024)",
              "UN Statistical Commission, 1999 (published 2000)",
              "Eurostat, HICP from 2000 to 2025 (5-digit subclasses from 2016)"),
  blurb = c(
    "The current international standard. 15 divisions: 01-13 cover household spending, 14 NPISHs and 15 general government. Adds explicit 'includes', 'also includes' and 'excludes' notes, a five-digit subclass level, and an optional FAO six-digit level for food. Eurostat's ECOICOP ver.2, used for the HICP from 2026, is the same structure code for code.",
    "The previous international standard, used in most household budget surveys and CPIs for two decades. 14 divisions: 01-12 household spending, 13 NPISHs and 14 general government; classes are the lowest level.",
    "Eurostat's European extension of COICOP 1999 used for the Harmonised Index of Consumer Prices until 2025. It keeps the COICOP 1999 divisions, groups and classes and adds five-digit subclasses. Explanatory notes are those of the parent COICOP 1999 class.")
)

meta <- list(
  built = format(Sys.Date()),
  downloaded = if (file.exists(raw("DOWNLOADED_ON.txt"))) readLines(raw("DOWNLOADED_ON.txt"))[1] else NA,
  type_labels = c(ND = "Non-durable goods", SD = "Semi-durable goods", D = "Durable goods",
                  S = "Services", Mixed = "Mixed"),
  ecoicop2_title_diffs = as.data.frame(e2_title_diffs)
)

out <- list(codes = as.data.frame(codes), corr = as.data.frame(cr),
            weights = as.data.frame(weights), infl = as.data.frame(infl),
            infl_all = as.data.frame(infl_all), monthly = as.data.frame(monthly),
            geos = as.data.frame(geos), versions = as.data.frame(versions), meta = meta)
dir.create("data", showWarnings = FALSE)
saveRDS(out, "data/coicop.rds", compress = "xz")

message(sprintf("codes: %s | corr: %d | weights: %d | infl: %d | monthly: %d | geos: %d",
                paste(names(table(codes$version)), table(codes$version), sep = "=", collapse = ", "),
                nrow(cr), nrow(weights), nrow(infl), nrow(monthly), nrow(geos)))
message("Wrote data/coicop.rds (", round(file.size("data/coicop.rds") / 1e6, 1), " MB)")
