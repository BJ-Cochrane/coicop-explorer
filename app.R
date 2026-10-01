# COICOP Explorer
# Search and compare COICOP 2018, COICOP 1999 and ECOICOP, with Eurostat HICP data.
# Run with shiny::runApp() from this folder, or Rscript run.R.

library(shiny)
library(bslib)
library(reactable)
library(plotly)
library(htmltools)

arial <- font_collection("Arial", "Helvetica", "sans-serif")
theme <- bs_theme(
  version = 5,
  bg = "#0e1217", fg = "#e8e6e1", primary = "#79a9ec",
  base_font = arial, heading_font = arial, code_font = arial,
  "font-size-base" = "0.9375rem",
  "border-radius" = "0", "border-radius-sm" = "0", "border-radius-lg" = "0", "border-radius-xl" = "0",
  "card-border-radius" = "0", "card-inner-border-radius" = "0",
  "link-decoration" = "none"
)

head_block <- function(title, ..., note = NULL) {
  card_header(div(class = "head", h2(class = "head-title", title), ...),
              if (!is.null(note)) div(class = "head-note", note))
}

table_theme <- function() {
  reactableTheme(
    color = "var(--ink)", backgroundColor = "transparent", borderColor = "var(--hair)",
    highlightColor = "var(--row-hover)",
    headerStyle = list(color = "var(--muted)", fontWeight = 600, fontSize = "0.7rem",
                       textTransform = "uppercase", letterSpacing = "0.08em",
                       borderBottom = "1px solid var(--ink)"),
    paginationStyle = list(color = "var(--muted)", fontSize = "0.8rem")
  )
}

# Explore ----

explore_ui <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    width = 300, open = list(desktop = "open", mobile = "closed"),
    div(class = "search",
        tags$label(`for` = "q", class = "eyebrow", "Search"),
        div(class = "search-box",
            tags$input(id = "q", type = "search", class = "form-control", autocomplete = "off",
                       placeholder = "A product, service or code"),
            tags$kbd("/"))),
    div(class = "try", span(class = "try-label", "Try"),
        lapply(EXAMPLES, \(x) tags$a(href = "#", class = "try-link", `data-q` = x, x))),
    checkboxGroupInput("versions", "Classifications", choices = VERSION_CHOICES, selected = VERSIONS$id),
    checkboxGroupInput("levels", "Levels", inline = TRUE, selected = 1:5,
                       choices = setNames(1:5, c("Division", "Group", "Class", "Subclass", "6-digit"))),
    checkboxGroupInput("types", "Type of product",
                       choices = setNames(c("ND", "SD", "D", "S"), META$type_labels[c("ND", "SD", "D", "S")])),
    input_switch("in_notes", "Search the explanatory notes", TRUE),
    p(class = "hint", "Codes work too: ", tags$code("07.1"), " or ", tags$code("CP0711"), ".")
  ),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(5, 7)), fill = TRUE, class = "explore-grid",
    card(
      class = "results-card", full_screen = TRUE,
      card_header(div(class = "head", uiOutput("results_title", inline = TRUE))),
      card_body(padding = 0, reactableOutput("results", height = "100%"))
    ),
    uiOutput("detail", fill = TRUE)
  )
)

# Structure ----

structure_ui <- div(
  class = "page",
  uiOutput("version_stats"),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(7, 5)),
    card(
      full_screen = TRUE,
      head_block("Shape of each classification",
                 div(class = "seg", radioButtons("s_version", NULL, VERSION_CHOICES, inline = TRUE))),
      card_body(
        div(class = "controls",
            div(class = "seg", radioButtons("s_kind", NULL, inline = TRUE,
                                            c(Sunburst = "sunburst", Treemap = "treemap", Icicle = "icicle"))),
            div(class = "seg", radioButtons("s_depth", "Depth", c(2, 3, 4, 5), selected = 3, inline = TRUE)),
            div(class = "seg", radioButtons("s_color", "Colour", c("Product type" = "type", Level = "level"),
                                            inline = TRUE))),
        uiOutput("type_legend"),
        plotlyOutput("hier", height = "620px"),
        p(class = "caption", "Segment size is the number of categories beneath it. Click a segment to open it.")
      )
    ),
    card(
      full_screen = TRUE,
      head_block("Lowest-level categories per division"),
      card_body(plotlyOutput("divbars", height = "700px"))
    )
  ),
  card(
    head_block("Categories at each level"),
    card_body(plotlyOutput("levels_chart", height = "320px"))
  )
)

# Compare ----

compare_ui <- div(
  class = "page",
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(4, 8)),
    card(
      head_block("Divisions side by side", note = "Shaded rows changed between versions."),
      card_body(padding = 0, reactableOutput("div_compare"))
    ),
    card(
      full_screen = TRUE,
      head_block("Where COICOP 2018 subclasses sit in COICOP 1999",
                 note = tagList(span(class = "key", style = "--k:var(--s-blue)", "same division number"),
                                span(class = "key", style = "--k:var(--s-orange)", "moved to another division"))),
      card_body(plotlyOutput("sankey", height = "640px"))
    )
  ),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(4, 8)),
    card(
      head_block("How 2018 subclasses map to 1999 classes"),
      card_body(
        plotlyOutput("relations", height = "220px"),
        tags$dl(class = "defs",
          tags$dt("One-to-one"), tags$dd("Maps to a single 1999 class that no other subclass shares."),
          tags$dt("Merge"), tags$dd("Several 2018 subclasses fold into one 1999 class."),
          tags$dt("Split"), tags$dd("One 2018 subclass draws on several 1999 classes."),
          tags$dt("Many-to-many"), tags$dd("Both at once."))
      )
    ),
    card(
      full_screen = TRUE,
      head_block("Correspondence table",
                 div(class = "filters",
                     textInput("c_q", NULL, placeholder = "Filter", width = "180px"),
                     selectInput("c_rel", NULL, width = "230px",
                                 choices = c("All relationships" = "", sort(unique(CORR$relation)))))),
      card_body(padding = 0, reactableOutput("corr_table", height = "520px"))
    )
  )
)

# Prices ----

prices_ui <- layout_sidebar(
  sidebar = sidebar(
    width = 280, open = list(desktop = "open", mobile = "closed"),
    radioButtons("p_version", "Classification",
                 c("COICOP 2018 (ECOICOP ver.2)" = "coicop2018", "ECOICOP ver.1, to 2025" = "ecoicop1")),
    selectInput("p_geo", "Country or area", GEO_CHOICES, selected = "EU27_2020"),
    sliderInput("p_year", "Year", min = 1996, max = LATEST_W[["coicop2018"]], value = LATEST_I[["coicop2018"]],
                step = 1, sep = "", ticks = FALSE),
    radioButtons("p_depth", "Detail", c(Divisions = 1, Groups = 2, Classes = 3), selected = 2),
    div(class = "hint",
        p("Weights are parts per thousand (‰) of the HICP basket, i.e. each category's share of ",
          "household spending."),
        p("Colour is the annual average inflation rate: blue for falling prices, red for rising, capped at ±10%."),
        p("COICOP 2018 series before 2026 are Eurostat back-calculations."))
  ),
  uiOutput("p_stats"),
  card(
    full_screen = TRUE,
    card_header(uiOutput("basket_title")),
    card_body(plotlyOutput("basket", height = "560px"))
  ),
  card(
    full_screen = TRUE,
    card_header(uiOutput("heat_title")),
    card_body(plotlyOutput("heat", height = "520px"))
  )
)

# About ----

about_ui <- div(
  class = "page about",
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(7, 5)),
    div(class = "article",
      p(class = "eyebrow", "About"),
      h1("What COICOP is"),
      p(class = "standfirst", HTML(paste0(
        "The <em>Classification of Individual Consumption According to Purpose</em> is the United Nations ",
        "standard for grouping what households spend money on by what the spending is for: food, housing, ",
        "transport, health and so on."))),
      p("It underpins household consumption in the national accounts, household budget and income and ",
        "expenditure surveys, consumer price indices including the EU's HICP, and the purchasing power ",
        "parities of the International Comparison Program."),
      h2("Reading a code"),
      p("Each level adds a digit after a full stop. In COICOP 2018:"),
      tags$table(class = "ladder",
        tags$tr(tags$td(class = "mono", "07"), tags$td("Division"), tags$td("Transport")),
        tags$tr(tags$td(class = "mono", "07.1"), tags$td("Group"), tags$td("Purchase of vehicles")),
        tags$tr(tags$td(class = "mono", "07.1.1"), tags$td("Class"), tags$td("Motor cars (D)")),
        tags$tr(tags$td(class = "mono", "07.1.1.1"), tags$td("Subclass"), tags$td("New motor cars (D)"))),
      p(HTML(paste0(
        "A category that is not split further gets a <code>0</code> (<code>02.2.0</code>). Eurostat drops ",
        "the dots and adds <code>CP</code>, so <code>07.1.1.1</code> is <code>CP07111</code>."))),
      h2("Type of product"),
      tags$dl(class = "types", lapply(c("ND", "SD", "D", "S"), \(t)
        tagList(tags$dt(type_badge(t)), tags$dd(META$type_labels[[t]])))),
      p(class = "muted", "Higher levels are marked mixed when the categories beneath them differ."),
      h2("Explanatory notes"),
      p(HTML(paste0(
        "<b>Includes</b> lists the core content of a category, <b>also includes</b> lists borderline items ",
        "that belong in it, and <b>excludes</b> lists things you might expect to find there but which are ",
        "classified elsewhere, with the code where they go. In this app those codes are links.")))
    ),
    div(class = "versions",
      lapply(seq_len(nrow(VERSIONS)), function(i) {
        v <- VERSIONS[i, ]
        n <- vapply(1:5, \(l) sum(CODES$version == v$id & CODES$level == l), 0)
        div(class = "version", style = sprintf("--vc:%s", v$color),
            div(class = "version-head", version_tag(v$id), h3(v$name)),
            p(class = "muted", paste0(v$publisher, ". ", v$adopted, ".")),
            p(v$blurb),
            p(class = "mono small", paste(n[n > 0], tolower(LEVEL_PLURAL[n > 0]), collapse = " · ")))
      })
    )
  ),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)),
    div(class = "article",
      h2("Sources"),
      tags$ul(class = "plain",
        tags$li("UN Statistics Division: COICOP 2018 structure and notes, COICOP 1999 notes, and the ",
                "2018 to 1999 correspondence table (final, 2024). ",
                tags$a(href = "https://unstats.un.org/unsd/classifications/Econ", target = "_blank",
                       "unstats.un.org")),
        tags$li("Eurostat: ECOICOP code lists, HICP item weights and inflation (prc_hicp_iw, prc_hicp_ainr, ",
                "prc_hicp_minr, prc_hicp_inw, prc_hicp_aind). ",
                tags$a(href = "https://ec.europa.eu/eurostat/web/hicp", target = "_blank", "ec.europa.eu"))),
      p(class = "muted", sprintf("Downloaded %s, built %s.", META$downloaded, META$built))
    ),
    div(class = "article",
      h2("Notes on the data"),
      tags$ul(class = "plain",
        tags$li("Eurostat's ECOICOP ver.2 uses the same 871 codes as COICOP 2018, so it is not listed ",
                "separately. Two food-detail titles differ (",
                paste(sprintf("%s, Eurostat “%s”", META$ecoicop2_title_diffs$code,
                              META$ecoicop2_title_diffs$eurostat), collapse = "; "),
                "); the UN titles are used."),
        tags$li("ECOICOP ver.1 has no notes of its own. The app shows those of the matching COICOP 1999 class."),
        tags$li("The COICOP 1999 notes were published as running text and have been split into lists ",
                "without changing the wording."),
        tags$li("HICP figures are missing where Eurostat publishes none: confidential, not applicable, or ",
                "outside the HICP, such as divisions 14 and 15."))
    )
  )
)

ui <- page_navbar(
  id = "nav",
  title = span(class = "brand", span(class = "brand-word", "COICOP"), span(class = "brand-sub", "Explorer")),
  window_title = "COICOP Explorer",
  theme = theme,
  fillable = "Explore",
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js")
  ),
  nav_panel("Explore", explore_ui),
  nav_panel("Structure", structure_ui),
  nav_panel("Compare versions", value = "Compare", compare_ui),
  nav_panel("Prices & weights", value = "Prices", prices_ui),
  nav_panel("About", about_ui),
  nav_spacer()
)

server <- function(input, output, session) {

  sel <- reactiveVal(list(v = "coicop2018", code = "01"))

  open_code <- function(v, code, switch_tab = FALSE) {
    if (is.null(v) || is.null(code) || !code_exists(v, code)) return()
    sel(list(v = v, code = code))
    if (switch_tab) nav_select("nav", "Explore")
  }

  # ?v=coicop2018&code=07.1.1 opens a category; the URL follows the selection
  observe({
    qs <- parseQueryString(session$clientData$url_search)
    if (!is.null(qs$code)) open_code(if (is.null(qs$v)) "coicop2018" else qs$v, qs$code)
  }) |> bindEvent(session$clientData$url_search, once = TRUE)
  observe({
    s <- sel()
    updateQueryString(sprintf("?v=%s&code=%s", s$v, s$code), mode = "replace")
  })

  observe(open_code(input$pick$v, input$pick$code)) |> bindEvent(input$pick)
  observe(open_code(input$goto$v, input$goto$code, switch_tab = TRUE)) |> bindEvent(input$goto)

  # Search results ----

  q <- debounce(reactive(if (is.null(input$q)) "" else input$q), 250)
  results <- reactive({
    req(input$versions)
    search_codes(q(), input$versions, input$levels, input$types, isTRUE(input$in_notes))
  })

  output$results_title <- renderUI({
    n <- nrow(results())
    if (!nzchar(trimws(q()))) return(h2(class = "head-title", "Divisions and groups"))
    h2(class = "head-title", format(n, big.mark = ","), if (n == 1) " match" else " matches",
       span(class = "head-q", sprintf(" for “%s”", trimws(q()))))
  })

  output$results <- renderReactable({
    d <- results()
    if (!nrow(d)) {
      return(reactable(data.frame(x = "Nothing found. Try fewer words, or turn on searching the notes."),
                       columns = list(x = colDef(name = "")), sortable = FALSE, theme = table_theme()))
    }
    d <- head(d, 400)
    s <- isolate(sel())
    d$title_html <- sprintf(
      '<div class="r-title%s">%s</div>%s%s',
      ifelse(d$level == 1, " r-div", ""),
      htmlEscape(d$title),
      ifelse(is.na(d$hit) | d$hit %in% c("Title", "Code"), "",
             sprintf('<span class="hit hit-%s">%s</span>', gsub(" ", "-", tolower(d$hit)), d$hit)),
      ifelse(is.na(d$snippet), "", sprintf('<div class="snip">%s</div>', d$snippet)))
    d$vhtml <- vapply(d$version, \(v) as.character(version_tag(v)), "")
    d$indent <- pmax(d$level - 1, 0)
    d$level_name <- sub("Food detail (6-digit)", "6-digit", d$level_name, fixed = TRUE)
    reactable(
      d[, c("version", "code", "vhtml", "title_html", "level_name", "indent")],
      columns = list(
        version = colDef(show = FALSE),
        indent = colDef(show = FALSE),
        vhtml = colDef(name = "", html = TRUE, width = 92),
        code = colDef(name = "Code", width = 116, class = "code-cell",
                      style = JS("function(r){ return {paddingLeft: (6 + r.values.indent*3) + 'px'} }")),
        title_html = colDef(name = "Category", html = TRUE, minWidth = 160),
        level_name = colDef(name = "Level", width = 80, class = "level-cell")
      ),
      onClick = JS("function(rowInfo){
        Shiny.setInputValue('pick', {v: rowInfo.values.version, code: rowInfo.values.code}, {priority: 'event'});
        document.querySelectorAll('#results .rt-tr.is-selected').forEach(function(el){ el.classList.remove('is-selected'); });
        var tr = window.event && window.event.target.closest('.rt-tr');
        if (tr) tr.classList.add('is-selected');
      }"),
      rowClass = JS(sprintf("function(r){ return (r.values.version === '%s' && r.values.code === '%s') ? 'is-selected' : '' }",
                            s$v, s$code)),
      highlight = TRUE, compact = TRUE, sortable = FALSE,
      pagination = TRUE, defaultPageSize = 50, paginationType = "simple", showPageInfo = TRUE,
      theme = table_theme()
    )
  })

  # Detail ----

  output$detail <- renderUI({
    s <- sel()
    row <- get_code(s$v, s$code)
    req(nrow(row) == 1)
    n_kids <- nrow(children_of(row$version, row$code))
    card(
      class = "detail", full_screen = TRUE, style = sprintf("--vc:%s", version_color(row$version)),
      card_header(class = "detail-head", detail_header(row)),
      card_body(
        class = "detail-body",
        navset_underline(
          id = "detail_tab", selected = isolate(input$detail_tab),
          nav_panel("Notes", value = "notes", detail_notes(row)),
          nav_panel(sprintf("Subdivisions (%d)", n_kids), value = "inside", detail_children(row)),
          nav_panel("Other versions", value = "corr", detail_corr(row)),
          nav_panel("Prices & weights", value = "hicp", detail_hicp(row))
        )
      )
    )
  })

  output$d_infl    <- renderPlotly({ s <- sel(); chart_infl_ts(s$v, s$code, "EU27_2020") })
  output$d_month   <- renderPlotly({ s <- sel(); chart_monthly(s$code) })
  output$d_wts     <- renderPlotly({ s <- sel(); chart_weight_ts(s$v, s$code) })
  output$d_country <- renderPlotly({ s <- sel(); chart_country_weights(s$v, s$code, LATEST_W[[s$v]]) })

  # Structure ----

  output$version_stats <- renderUI({
    div(class = "stats", lapply(seq_len(nrow(VERSIONS)), function(i) {
      v <- VERSIONS[i, ]
      n <- vapply(1:5, \(l) sum(CODES$version == v$id & CODES$level == l), 0)
      stat_tile(v$name, format(sum(n), big.mark = ","),
                paste(n[n > 0], tolower(LEVEL_PLURAL[n > 0]), collapse = " · "), color = v$color)
    }))
  })

  observe({
    mx <- max(CODES$level[CODES$version == input$s_version])
    updateRadioButtons(session, "s_depth", choices = seq(2, mx), inline = TRUE,
                       selected = min(as.integer(input$s_depth), mx))
  }) |> bindEvent(input$s_version)

  output$type_legend <- renderUI({
    if (input$s_color != "type") return(NULL)
    tc <- type_colors()
    div(class = "legend", lapply(names(tc), \(t) span(class = "key", style = sprintf("--k:%s", tc[[t]]),
                                                      META$type_labels[[t]])))
  })
  output$hier <- renderPlotly({
    depth <- min(as.integer(input$s_depth), max(CODES$level[CODES$version == input$s_version]))
    chart_hierarchy(input$s_version, input$s_kind, input$s_color, depth)
  })
  output$divbars <- renderPlotly(chart_division_bars(input$s_version))
  output$levels_chart <- renderPlotly(chart_levels())
  observe({
    cd <- event_data("plotly_click", source = "hier")$customdata
    if (!is.null(cd) && nzchar(cd)) open_code(input$s_version, cd, switch_tab = TRUE)
  })

  # Compare ----

  output$div_compare <- renderReactable({
    d18 <- CODES[CODES$version == "coicop2018" & CODES$level == 1, c("code", "title")]
    d99 <- CODES[CODES$version == "coicop1999" & CODES$level == 1, c("code", "title")]
    d <- merge(d18, d99, by = "code", all = TRUE, suffixes = c("18", "99"))
    d$changed <- is.na(d$title18) | is.na(d$title99) | d$title18 != d$title99
    reactable(
      d, compact = TRUE, sortable = FALSE, pagination = FALSE,
      columns = list(
        code = colDef(name = "", width = 44, class = "mono"),
        title18 = colDef(name = "COICOP 2018", cell = \(x, i) if (is.na(x)) "–" else code_link("coicop2018", d$code[i], x)),
        title99 = colDef(name = "COICOP 1999", cell = \(x, i) if (is.na(x)) "–" else code_link("coicop1999", d$code[i], x)),
        changed = colDef(show = FALSE)
      ),
      rowClass = \(i) if (d$changed[i]) "row-changed" else "",
      theme = table_theme()
    )
  })
  output$sankey <- renderPlotly(chart_sankey())
  output$relations <- renderPlotly(chart_relations())
  output$corr_table <- renderReactable({
    d <- CORR
    if (nzchar(input$c_rel)) d <- d[d$relation == input$c_rel, ]
    qq <- tolower(trimws(input$c_q))
    if (nzchar(qq)) d <- d[grepl(qq, tolower(paste(d$code2018, d$title2018, d$code1999, d$title1999, d$note)), fixed = TRUE), ]
    d <- d[, c("code2018", "title2018", "code1999", "title1999", "relation", "note")]
    reactable(
      d, compact = TRUE, highlight = TRUE, defaultPageSize = 25, paginationType = "simple",
      columns = list(
        code2018 = colDef(name = "2018", width = 96, class = "mono", cell = \(x) code_link("coicop2018", x)),
        title2018 = colDef(name = "COICOP 2018", minWidth = 180),
        code1999 = colDef(name = "1999", width = 80, class = "mono", cell = \(x) code_link("coicop1999", x)),
        title1999 = colDef(name = "COICOP 1999", minWidth = 160),
        relation = colDef(name = "Relationship", width = 150, class = "small"),
        note = colDef(name = "Common content", minWidth = 140, class = "small", na = "")
      ),
      theme = table_theme()
    )
  })

  # Prices ----

  observe({
    v <- input$p_version
    yrs <- range(WEIGHTS$year[WEIGHTS$version == v])
    updateSliderInput(session, "p_year", min = yrs[1], max = yrs[2], value = min(input$p_year, LATEST_I[[v]]))
  }) |> bindEvent(input$p_version)

  output$p_stats <- renderUI({
    v <- input$p_version; g <- input$p_geo; y <- input$p_year
    a <- INFL_ALL$value[INFL_ALL$version == v & INFL_ALL$geo == g & INFL_ALL$year == y]
    w <- WEIGHTS[WEIGHTS$version == v & WEIGHTS$geo == g & WEIGHTS$year == y & nchar(WEIGHTS$code) == 2, ]
    i <- INFL[INFL$version == v & INFL$geo == g & INFL$year == y & nchar(INFL$code) == 2, ]
    top <- if (nrow(w)) w[which.max(w$value), ]
    hot <- if (nrow(i)) i[which.max(i$value), ]
    ttl <- \(cd) CODES$title[CODES$version == v & CODES$code == cd]
    div(class = "stats",
      stat_tile(paste("All-items inflation,", y), if (length(a)) sprintf("%+.1f%%", a) else "–", geo_label(g)),
      stat_tile("Largest share of spending", if (!is.null(top)) sprintf("%.1f%%", top$value / 10) else "–",
                if (!is.null(top)) paste(top$code, ttl(top$code)) else "No data"),
      stat_tile("Fastest-rising division", if (!is.null(hot)) sprintf("%+.1f%%", hot$value) else "–",
                if (!is.null(hot)) paste(hot$code, ttl(hot$code)) else "No data"))
  })
  output$basket_title <- renderUI(div(class = "head",
    h2(class = "head-title", sprintf("The basket: %s, %s", geo_label(input$p_geo), input$p_year)),
    span(class = "head-note", "Area is weight, colour is inflation. Click to zoom.")))
  output$basket <- renderPlotly(chart_basket(input$p_version, input$p_geo, input$p_year, as.integer(input$p_depth)))
  output$heat_title <- renderUI(div(class = "head",
    h2(class = "head-title", sprintf("Inflation by division and year: %s", geo_label(input$p_geo))),
    span(class = "head-note", "Click a row to open the division.")))
  output$heat <- renderPlotly(chart_heatmap(input$p_version, input$p_geo))
  observe({
    e <- event_data("plotly_click", source = "heat")
    if (!is.null(e$y)) open_code(input$p_version, sub(" .*$", "", e$y), switch_tab = TRUE)
  })
}

shinyApp(ui, server)
