# Plotly charts, styled for the dark theme.

pal <- function() list(
  ink = "#e8e6e1", ink2 = "#b9bcc2", muted = "#858b94", grid = "#262c35", axis = "#3a414c",
  surface = "#151a21", hover = "#1d232c",
  s = c(blue = "#3987e5", orange = "#d95926", aqua = "#199e70", yellow = "#c98500"),
  mid = "#2a3039", neutral = "#5d646e",
  div = list(c(0, "#184f95"), c(0.5, "#2a3039"), c(1, "#e66767"))
)

version_colors <- function() {
  p <- pal()$s
  c(coicop2018 = p[["blue"]], coicop1999 = p[["orange"]], ecoicop1 = p[["aqua"]])
}
type_colors <- function() {
  p <- pal()
  c(ND = p$s[["blue"]], SD = p$s[["orange"]], D = p$s[["aqua"]], S = p$s[["yellow"]],
    Mixed = p$neutral)
}

style_plot <- function(p, legend = FALSE, ...) {
  k <- pal()
  ax <- list(gridcolor = k$grid, linecolor = k$axis, zerolinecolor = k$axis,
             tickfont = list(color = k$muted, size = 11), title = list(font = list(color = k$ink2, size = 12)),
             automargin = TRUE)
  defaults <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Arial, Helvetica, sans-serif", color = k$ink2, size = 12),
    xaxis = ax, yaxis = ax,
    showlegend = legend,
    legend = list(orientation = "h", x = 0, y = 1.12, font = list(color = k$ink2)),
    hoverlabel = list(bgcolor = k$hover, bordercolor = k$ink, font = list(color = k$ink, size = 12)),
    margin = list(l = 8, r = 8, t = 8, b = 8)
  )
  do.call(plotly::layout, c(list(p), utils::modifyList(defaults, list(...)))) |>
    plotly::config(displaylogo = FALSE, displayModeBar = FALSE, responsive = TRUE)
}

no_data_plot <- function(msg) {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    style_plot(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = msg, showarrow = FALSE, xref = "paper", yref = "paper",
                              x = 0.5, y = 0.5, font = list(color = pal()$muted, size = 13))))
}

wrap_title <- function(x, n = 28) vapply(x, function(s) paste(strwrap(s, n), collapse = "<br>"), "")

# Structure ----

chart_hierarchy <- function(v, kind = "sunburst", color_by = "type", depth = 3) {
  d <- CODES[CODES$version == v & CODES$level <= depth, ]
  root <- version_name(v)
  # node size = number of categories at the chosen depth beneath it
  leaves <- d[d$level == depth | d$n_children == 0, ]
  leaves <- leaves[!duplicated(leaves$code), ]
  d$value <- vapply(d$code, function(cd) sum(leaves$code == cd | startsWith(leaves$code, paste0(cd, "."))), 0)
  ids <- c(root, d$code)
  parents <- c("", ifelse(is.na(d$parent), root, d$parent))
  labels <- c(root, ifelse(d$level == 1, paste(d$code, wrap_title(d$title, 22)), d$code))
  values <- c(sum(d$value[d$level == 1]), d$value)
  hover <- c(root, sprintf("<b>%s</b> %s<br>%s<br>%d %s below", d$code, htmltools::htmlEscape(d$title),
                           d$level_name, d$value, tolower(LEVEL_PLURAL[depth])))
  if (color_by == "type") {
    tc <- type_colors()
    cols <- c(pal()$mid, ifelse(is.na(d$type), pal()$neutral, tc[d$type]))
  } else {
    steps <- c("#104281", "#1c5cab", "#3987e5", "#6da7ec", "#9ec5f4")
    cols <- c(pal()$mid, steps[d$level])
  }
  plotly::plot_ly(
    type = kind, ids = ids, parents = parents, labels = labels, values = values,
    branchvalues = "total", sort = FALSE, hovertext = hover, hoverinfo = "text",
    customdata = c("", d$code), source = "hier", maxdepth = 3,
    marker = list(colors = cols, line = list(color = pal()$surface, width = 1.5)),
    insidetextfont = list(size = 11),
    textinfo = "label"
  ) |>
    style_plot(margin = list(l = 0, r = 0, t = 0, b = 0)) |>
    plotly::event_register("plotly_click")
}

chart_levels <- function() {
  vc <- version_colors()
  tab <- as.data.frame(table(CODES$version, CODES$level_name))
  names(tab) <- c("version", "level", "n")
  tab <- tab[tab$n > 0, ]
  lv <- c("Division", "Group", "Class", "Subclass", "Food detail (6-digit)")
  tab$level <- factor(tab$level, levels = lv)
  p <- plotly::plot_ly()
  for (v in VERSIONS$id) {
    s <- tab[tab$version == v, ]
    p <- p |> plotly::add_bars(x = s$level, y = s$n, name = version_name(v),
                               marker = list(color = vc[[v]]),
                               text = s$n, textposition = "outside", textfont = list(color = pal()$ink2),
                               hovertemplate = paste0("<b>", version_name(v), "</b><br>%{x}: %{y}<extra></extra>"))
  }
  p |> style_plot(legend = TRUE, barmode = "group", bargap = 0.25,
                  yaxis = list(title = list(text = "Categories"), gridcolor = pal()$grid,
                               tickfont = list(color = pal()$muted)),
                  xaxis = list(title = list(text = NULL), tickfont = list(color = pal()$ink2)))
}

chart_division_bars <- function(v) {
  d <- CODES[CODES$version == v, ]
  low <- max(d$level[d$level <= 4])
  leaves <- d[d$level == low, ]
  divs <- d[d$level == 1, ]
  tc <- type_colors()
  types <- intersect(c("ND", "SD", "D", "S", "Mixed"), unique(leaves$type))
  p <- plotly::plot_ly()
  ylab <- paste(divs$code, short_text(divs$title, 38))
  for (t in types) {
    n <- vapply(divs$division, function(x) sum(leaves$division == x & leaves$type %in% t), 0)
    p <- p |> plotly::add_bars(y = ylab, x = n, name = META$type_labels[[t]], orientation = "h",
                               marker = list(color = tc[[t]], line = list(color = pal()$surface, width = 1)),
                               hovertemplate = paste0("%{y}<br>", META$type_labels[[t]], ": %{x}<extra></extra>"))
  }
  p |> style_plot(legend = TRUE, barmode = "stack", bargap = 0.3,
                  yaxis = list(categoryorder = "array", categoryarray = rev(ylab),
                               tickfont = list(color = pal()$ink2, size = 11), gridcolor = "rgba(0,0,0,0)"),
                  xaxis = list(title = list(text = paste(LEVEL_PLURAL[low], "per division")),
                               gridcolor = pal()$grid, tickfont = list(color = pal()$muted))) |>
    plotly::layout(legend = list(orientation = "h", x = 0, y = 1, yanchor = "bottom", traceorder = "normal",
                                 font = list(color = pal()$ink2)),
                   margin = list(l = 8, r = 8, t = 36, b = 8))
}

# Correspondence ----

chart_sankey <- function() {
  k <- pal()
  cr <- CORR[CORR$level2018 == 4 & CORR$level1999 == 3, ]
  cr$d18 <- substr(cr$code2018, 1, 2); cr$d99 <- substr(cr$code1999, 1, 2)
  agg <- aggregate(list(n = cr$code2018), by = list(d18 = cr$d18, d99 = cr$d99), FUN = length)
  t18 <- CODES[CODES$version == "coicop2018" & CODES$level == 1, c("code", "title")]
  t99 <- CODES[CODES$version == "coicop1999" & CODES$level == 1, c("code", "title")]
  n18 <- sort(unique(agg$d18)); n99 <- sort(unique(agg$d99))
  lab <- c(paste(n18, short_text(t18$title[match(n18, t18$code)], 42)),
           paste(n99, short_text(t99$title[match(n99, t99$code)], 42)))
  # fix node positions so both sides stay in code order
  tot18 <- tapply(agg$n, agg$d18, sum)[n18]; tot99 <- tapply(agg$n, agg$d99, sum)[n99]
  ypos <- function(tot) { f <- tot / sum(tot); gap <- 0.012; mid <- cumsum(f) - f / 2
    pmin(pmax(mid * (1 - gap * length(f)) + gap * (seq_along(f) - 0.5), 0.001), 0.999) }
  node_x <- c(rep(0.001, length(n18)), rep(0.999, length(n99)))
  node_y <- c(ypos(tot18), ypos(tot99))
  src <- match(agg$d18, n18) - 1; tgt <- length(n18) + match(agg$d99, n99) - 1
  alpha <- function(hex, a) { r <- grDevices::col2rgb(hex); sprintf("rgba(%d,%d,%d,%s)", r[1], r[2], r[3], a) }
  lcol <- ifelse(agg$d18 == agg$d99, alpha(k$s[["blue"]], 0.5), alpha(k$s[["orange"]], 0.65))
  plotly::plot_ly(
    type = "sankey", arrangement = "snap",
    node = list(label = lab, pad = 8, thickness = 14, x = node_x, y = unname(node_y),
                color = c(rep(version_colors()[["coicop2018"]], length(n18)),
                          rep(version_colors()[["coicop1999"]], length(n99))),
                line = list(color = k$surface, width = 0.5),
                hovertemplate = "%{label}<br>%{value} subclass links<extra></extra>"),
    link = list(source = src, target = tgt, value = agg$n, color = lcol,
                hovertemplate = "%{source.label} → %{target.label}<br>%{value} links<extra></extra>"),
    textfont = list(color = k$ink, size = 11)
  ) |> style_plot(margin = list(l = 4, r = 4, t = 4, b = 4))
}

chart_relations <- function() {
  k <- pal()
  cr <- CORR[CORR$level2018 == 4 & CORR$level1999 == 3, ]
  rel <- vapply(split(cr$relation, cr$code2018), function(r) r[1], "")
  tab <- sort(table(rel))
  plotly::plot_ly(y = names(tab), x = as.vector(tab), type = "bar", orientation = "h",
                  marker = list(color = k$s[["blue"]]),
                  text = as.vector(tab), textposition = "outside", textfont = list(color = k$ink2),
                  hovertemplate = "%{y}: %{x} subclasses<extra></extra>") |>
    style_plot(xaxis = list(visible = FALSE), bargap = 0.35,
               yaxis = list(tickfont = list(color = k$ink2), gridcolor = "rgba(0,0,0,0)"))
}

# HICP ----

hicp_series <- function(tbl, v, code, geos) {
  tbl[tbl$version == v & tbl$code == code & tbl$geo %in% geos, ]
}

chart_weight_ts <- function(v, code) {
  k <- pal()
  d <- hicp_series(WEIGHTS, v, code, c("EU27_2020", "EA"))
  if (!nrow(d)) return(no_data_plot("No HICP weights published for this category"))
  p <- plotly::plot_ly()
  cols <- c(EU27_2020 = k$s[["blue"]], EA = k$s[["orange"]])
  for (g in c("EU27_2020", "EA")) {
    s <- d[d$geo == g, ]; s <- s[order(s$year), ]
    if (!nrow(s)) next
    p <- p |> plotly::add_trace(x = s$year, y = s$value, type = "scatter", mode = "lines",
                                name = geo_label(g), line = list(color = cols[[g]], width = 2),
                                hovertemplate = paste0("<b>", geo_label(g), "</b> %{x}<br>%{y:.2f}‰<extra></extra>"))
  }
  shapes <- if (v == "coicop2018") list(list(type = "rect", xref = "x", yref = "paper", x0 = 1995.5, x1 = 2025.5,
                                            y0 = 0, y1 = 1, fillcolor = k$grid, opacity = 0.35, line = list(width = 0),
                                            layer = "below")) else NULL
  p |> style_plot(legend = TRUE, hovermode = "x unified", shapes = shapes,
                  yaxis = list(title = list(text = "Weight (‰ of total)"), rangemode = "tozero",
                               gridcolor = k$grid, tickfont = list(color = k$muted)),
                  xaxis = list(gridcolor = "rgba(0,0,0,0)", tickfont = list(color = k$muted)))
}

chart_country_weights <- function(v, code, year) {
  k <- pal()
  d <- WEIGHTS[WEIGHTS$version == v & WEIGHTS$code == code & WEIGHTS$year == year, ]
  if (!nrow(d)) return(no_data_plot(paste("No", year, "weights for this category")))
  d$label <- geo_label(d$geo)
  d <- d[order(d$value), ]
  agg <- d$geo %in% c("EU27_2020", "EA")
  plotly::plot_ly(y = factor(d$label, levels = d$label), x = d$value, type = "bar", orientation = "h",
                  marker = list(color = ifelse(agg, k$s[["orange"]], k$s[["blue"]])),
                  hovertemplate = "<b>%{y}</b><br>%{x:.2f}‰ of HICP basket<extra></extra>") |>
    style_plot(bargap = 0.25,
               xaxis = list(title = list(text = paste0("Weight in ", year, " (‰)")), gridcolor = k$grid,
                            tickfont = list(color = k$muted), side = "top"),
               yaxis = list(tickfont = list(color = k$ink2, size = 10), gridcolor = "rgba(0,0,0,0)"))
}

chart_infl_ts <- function(v, code, geo = "EU27_2020") {
  k <- pal()
  d <- hicp_series(INFL, v, code, geo); d <- d[order(d$year), ]
  a <- INFL_ALL[INFL_ALL$version == v & INFL_ALL$geo == geo, ]; a <- a[order(a$year), ]
  if (!nrow(d)) return(no_data_plot("No HICP price index published for this category"))
  plotly::plot_ly() |>
    plotly::add_trace(x = a$year, y = a$value, type = "scatter", mode = "lines", name = "All items",
                      line = list(color = k$neutral, width = 2, dash = "dot"),
                      hovertemplate = "All items %{x}: %{y:.1f}%<extra></extra>") |>
    plotly::add_trace(x = d$year, y = d$value, type = "scatter", mode = "lines", name = "This category",
                      line = list(color = k$s[["blue"]], width = 2),
                      hovertemplate = "This category %{x}: %{y:.1f}%<extra></extra>") |>
    style_plot(legend = TRUE, hovermode = "x unified",
               yaxis = list(title = list(text = "Annual average rate (%)"), gridcolor = k$grid,
                            zerolinecolor = k$axis, tickfont = list(color = k$muted)),
               xaxis = list(gridcolor = "rgba(0,0,0,0)", tickfont = list(color = k$muted)))
}

chart_monthly <- function(code) {
  k <- pal()
  d <- MONTHLY[MONTHLY$code == code, ]
  if (!nrow(d)) return(no_data_plot("No monthly series for this category"))
  cols <- c(EU27_2020 = k$s[["blue"]], EA = k$s[["orange"]])
  p <- plotly::plot_ly()
  for (g in c("EU27_2020", "EA")) {
    s <- d[d$geo == g, ]; s <- s[order(s$month), ]
    p <- p |> plotly::add_trace(x = s$month, y = s$value, type = "scatter", mode = "lines", name = geo_label(g),
                                line = list(color = cols[[g]], width = 2),
                                hovertemplate = paste0(geo_label(g), ": %{y:.1f}%<extra></extra>"))
  }
  p |> style_plot(legend = TRUE, hovermode = "x unified",
                  yaxis = list(title = list(text = "Year-on-year change (%)"), gridcolor = k$grid,
                               zerolinecolor = k$axis, tickfont = list(color = k$muted)),
                  xaxis = list(gridcolor = "rgba(0,0,0,0)", tickfont = list(color = k$muted)))
}

# Basket treemap: area is weight, colour is that year's inflation
chart_basket <- function(v, geo, year, depth = 2) {
  k <- pal()
  w <- WEIGHTS[WEIGHTS$version == v & WEIGHTS$geo == geo & WEIGHTS$year == year, ]
  cd <- CODES[CODES$version == v, ]
  w <- w[w$code %in% cd$code[cd$level == depth], ]
  if (!nrow(w)) return(no_data_plot("No weights for this selection"))
  i <- INFL[INFL$version == v & INFL$geo == geo & INFL$year == year, ]
  nodes <- unique(unlist(lapply(w$code, function(x) c(ancestors_of(x), x))))
  val <- vapply(nodes, function(n) sum(w$value[w$code == n | startsWith(w$code, paste0(n, "."))]), 0)
  rate <- i$value[match(nodes, i$code)]
  ttl <- cd$title[match(nodes, cd$code)]
  par <- vapply(nodes, function(n) { a <- ancestors_of(n); if (length(a)) a[length(a)] else "all" }, "")
  all_rate <- INFL_ALL$value[INFL_ALL$version == v & INFL_ALL$geo == geo & INFL_ALL$year == year]
  nodes <- c("all", nodes); par <- c("", par); ttl <- c("All items", ttl)
  val <- c(sum(val[par[-1] == "all"]), val); rate <- c(if (length(all_rate)) all_rate else NA, rate)
  lim <- 10
  plotly::plot_ly(
    type = "treemap", ids = nodes, parents = par, values = val, branchvalues = "total", sort = TRUE,
    labels = ifelse(nodes == "all", "<b>All items</b>", paste0("<b>", nodes, "</b> ", wrap_title(ttl, 26))),
    customdata = nodes, source = "basket",
    hovertext = sprintf("<b>%s</b> %s<br>Weight: %.1f‰ (%.1f%%)<br>Inflation %s: %s",
                        ifelse(nodes == "all", "", nodes), htmltools::htmlEscape(ttl), val, val / 10, year,
                        ifelse(is.na(rate), "n/a", sprintf("%+.1f%%", rate))),
    hoverinfo = "text", textinfo = "label",
    marker = list(colors = pmax(pmin(ifelse(is.na(rate), 0, rate), lim), -lim), colorscale = k$div,
                  cmin = -lim, cmax = lim, cmid = 0, showscale = TRUE,
                  line = list(color = k$surface, width = 2),
                  colorbar = list(title = list(text = "Inflation %", font = list(color = k$ink2, size = 11)),
                                  tickfont = list(color = k$muted), thickness = 10, len = 0.6,
                                  ticksuffix = "%")),
    textfont = list(color = k$ink),
    pathbar = list(visible = TRUE, textfont = list(color = k$ink2))
  ) |> style_plot(margin = list(l = 0, r = 0, t = 24, b = 0)) |>
    plotly::event_register("plotly_click")
}

chart_heatmap <- function(v, geo, level = 1) {
  k <- pal()
  cd <- CODES[CODES$version == v & CODES$level == level, ]
  d <- INFL[INFL$version == v & INFL$geo == geo & INFL$code %in% cd$code, ]
  if (!nrow(d)) return(no_data_plot("No inflation series for this selection"))
  yrs <- sort(unique(d$year)); codes <- cd$code[cd$code %in% d$code]
  z <- matrix(NA_real_, length(codes), length(yrs))
  z[cbind(match(d$code, codes), match(d$year, yrs))] <- d$value
  ylab <- paste(codes, short_text(cd$title[match(codes, cd$code)], 34))
  lim <- 10
  plotly::plot_ly(x = yrs, y = ylab, z = z, type = "heatmap", colorscale = k$div,
                  zmin = -lim, zmax = lim, zmid = 0, xgap = 2, ygap = 2,
                  customdata = matrix(rep(codes, length(yrs)), ncol = length(yrs)), source = "heat",
                  hovertemplate = "<b>%{y}</b><br>%{x}: %{z:.1f}%<extra></extra>",
                  colorbar = list(title = list(text = "%", font = list(color = k$ink2)), thickness = 10,
                                  tickfont = list(color = k$muted), ticksuffix = "%", len = 0.8)) |>
    style_plot(yaxis = list(autorange = "reversed", tickfont = list(color = k$ink2, size = 11),
                                  gridcolor = "rgba(0,0,0,0)"),
               xaxis = list(tickfont = list(color = k$muted), gridcolor = "rgba(0,0,0,0)", dtick = 2))
}
