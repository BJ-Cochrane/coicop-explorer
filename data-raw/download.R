# Downloads the UN and Eurostat source files into data-raw/.
# Rscript data-raw/download.R, then Rscript data-raw/build_data.R
#
# Eurostat often drops or queues large requests, so get() keeps retrying.
# SKIP_EXISTING=1 keeps files that are already complete. Uses curl, or httr2 if curl is missing.

options(timeout = 600)
dir  <- "data-raw"
ua   <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) coicop-explorer"
skip <- identical(Sys.getenv("SKIP_EXISTING"), "1")

looks_valid <- function(path, gz) {
  if (!file.exists(path) || file.size(path) < 1000) return(FALSE)
  if (!gz) return(TRUE)
  if (!identical(readBin(path, "raw", 2), as.raw(c(0x1f, 0x8b)))) return(FALSE)
  # truncated files still start with the gzip header, so read to the end
  tryCatch({
    con <- gzfile(path); on.exit(close(con))
    length(readLines(con, warn = TRUE)) > 1
  }, warning = \(w) FALSE, error = \(e) FALSE)
}

get <- function(url, file, tries = 12, wait = 30) {
  dest <- file.path(dir, file)
  gz   <- grepl("\\.gz$", file)
  if (skip && looks_valid(dest, gz)) return(message("== ", file, " (kept)"))
  message("-> ", file)
  curl_cli <- Sys.which("curl")
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      # R's libcurl on Windows gets "connection reset" from Eurostat; curl.exe doesn't
      if (nzchar(curl_cli)) {
        status <- system2(curl_cli, c("-sSL", "-m", "600", "-A", shQuote(ua),
                                      "-o", shQuote(dest), shQuote(url)))
        if (status != 0) stop("curl exit ", status)
      } else {
        httr2::request(url) |>
          httr2::req_user_agent(ua) |>
          httr2::req_error(is_error = \(r) FALSE) |>
          httr2::req_timeout(600) |>
          httr2::req_perform(path = dest)
      }
      looks_valid(dest, gz)
    }, error = \(e) FALSE)
    if (ok) return(invisible(dest))
    message("   not ready (attempt ", i, "), retrying in ", wait, "s")
    Sys.sleep(wait)
  }
  stop("Could not download ", file)
}

# UN Statistics Division ----
unsd <- "https://unstats.un.org/unsd/classifications/Econ/Download"
get(file.path(unsd, "COICOP_2018_English_structure.xlsx"),
    "COICOP_2018_English_structure.xlsx")
# this workbook also holds the COICOP 1999 notes
get(file.path(unsd, "COICOP2018_COICOP1999_correspondence_table_final.xlsx"),
    "COICOP2018_COICOP1999_correspondence_table_final.xlsx")

# Eurostat code lists: COICOP is ECOICOP ver.1, COICOP18 is ver.2 ----
sdmx <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1"
for (cl in c("COICOP", "COICOP18", "GEO"))
  get(sprintf("%s/codelist/ESTAT/%s?format=TSV&lang=en", sdmx, cl),
      sprintf("eurostat_%s_codelist.tsv", cl))

# Eurostat HICP: item weights and annual rates for ver.2 (iw, ainr) and ver.1 (inw, aind) ----
for (ds in c("prc_hicp_iw", "prc_hicp_ainr", "prc_hicp_inw", "prc_hicp_aind"))
  get(sprintf("%s/data/%s?format=TSV&compressed=true", sdmx, ds),
      sprintf("%s.tsv.gz", ds))
# monthly rates for the EU and euro area only; the full table is ~22M cells
get(sprintf("%s/data/prc_hicp_minr/M.RCH_A..EU27_2020+EA?format=TSV&compressed=true&startPeriod=2015-01", sdmx),
    "prc_hicp_minr_EU_EA.tsv.gz")

writeLines(format(Sys.time(), "%Y-%m-%d"), file.path(dir, "DOWNLOADED_ON.txt"))
message("Done.")
