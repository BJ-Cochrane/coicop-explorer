# Makes the README screenshots. Rscript docs/make_screenshots.R
# Needs chromote, callr, magick and a Chrome or Edge install.

library(chromote)

port <- 4399
app <- callr::r_bg(function(port) shiny::runApp(".", port = port, launch.browser = FALSE),
                   args = list(port = port))
on.exit(app$kill(), add = TRUE)
Sys.sleep(8)

b <- ChromoteSession$new(width = 1440, height = 900)

js <- function(code) b$Runtime$evaluate(code, awaitPromise = TRUE)
idle <- "new Promise(r => { const t = () => document.documentElement.classList.contains('shiny-busy') ? setTimeout(t, 200) : setTimeout(r, 800); setTimeout(t, 500); })"

shot <- function(file, query = "", steps = character(), wait = 2, height = 900) {
  b$Emulation$setDeviceMetricsOverride(width = 1440, height = height, deviceScaleFactor = 1, mobile = FALSE)
  b$Page$navigate(sprintf("http://127.0.0.1:%d/%s", port, query))
  b$Page$loadEventFired()
  js(idle)
  for (s in steps) { js(s); js(idle) }
  Sys.sleep(wait)
  png <- file.path("docs", paste0(file, ".png"))
  b$screenshot(png, selector = "html", scale = 1, show = FALSE)
  img <- magick::image_read(png)
  magick::image_write(img, file.path("docs", paste0(file, ".webp")), format = "webp", quality = 88)
  unlink(png)
  message("saved docs/", file, ".webp")
}

tab   <- function(v) sprintf("document.querySelector('a[data-value=\"%s\"]').click()", v)
query <- function(q) sprintf("$('#q').val('%s').trigger('input')", q)
dtab  <- function(v) sprintf("document.querySelector('[data-value=\"%s\"]').click()", v)

shot("explore",   "?v=coicop2018&code=01.1.1.1", query("rice"))
shot("detail-prices", "?v=coicop2018&code=04.5.1", c(query("electricity"), dtab("hicp")))
shot("structure", "", tab("Structure"), height = 1180)
shot("compare",   "", tab("Compare"))
shot("basket",    "", tab("Prices"))

b$close()
