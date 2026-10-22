# ============================================================================
# SERVER
# ============================================================================
# The app's logic. Sections:
#   1. CITY / AGE-GROUP FILTERS
#   2. HEAT-HEALTH MAPPING TAB   (map, click + hover, RR curve, detail box, small maps)
#   3. DATA EXPLORER TAB      (layer stack: on/off, order, opacity)
#
# Remember the two layers are kept separate:
#   filtered_data() = health polygons  (all the numbers live here)
#   city_barrios()  = barrio outlines  (names only, never carry health data)
# ============================================================================

server <- function(input, output, session) {

  # Language from the URL (?lang=pt), the same one the page was built in.
  # tr("key") gives the text for a key in data/translations.xlsx.
  lang <- get_language(isolate(session$clientData$url_search))
  tr <- make_tr(lang)

  # --------------------------------------------------------------------
  # LANDING PAGE (pop-up shown when the app opens)
  # --------------------------------------------------------------------
  # The text is landing_title / landing_body in data/translations.xlsx.
  showModal(modalDialog(
    title = tr("landing_title"),
    text_paragraphs(tr("landing_body")),
    easyClose = TRUE, size = "l",
    footer = modalButton(tr("landing_button"))
  ))

  # --------------------------------------------------------------------
  # 1. CITY / AGE-GROUP FILTERS
  # --------------------------------------------------------------------

  # Age-group choices depend on the city, so this dropdown is built here.
  output$age_group_ui <- renderUI({
    choices <- age_group_choices(input$city, tr)
    selectInput("age_group", tr("label_age_group"), choices = choices, selected = "all")
  })

  # Health polygons for the chosen city + age group.
  filtered_data <- reactive({
    req(input$city, input$age_group)
    health_sf |> filter(city == input$city, age_group == input$age_group)
  })

  # Barrio outlines for the chosen city.
  city_barrios <- reactive({
    req(input$city)
    barrios_sf |> filter(city == input$city)
  })


  # --------------------------------------------------------------------
  # 2. HEAT-HEALTH MAPPING TAB
  # --------------------------------------------------------------------

  # ---- The main map(s): health polygons (no outline) + barrio outlines on top.
  # "Total EDF" draws one map (edf). "Split" draws two side by side: edf_dE on
  # the left, edf_dT on the right, on ONE shared colour scale.
  # Basemap choices for the big maps: Leaflet's layer-switcher icon (top right of
  # each map) lets the user pick between them. The first is the default.
  add_basemaps <- function(map) {
    map |>
      addProviderTiles(providers$OpenStreetMap, group = tr("basemap_street")) |>
      # Short credits (the providers' default credits are very long). Esri asks for
      # credit to Esri and its imagery sources; keep at least this much.
      addProviderTiles(providers$Esri.WorldImagery, group = tr("basemap_satellite"),
                       options = providerTileOptions(attribution = "Imagery © Esri, Maxar, Earthstar Geographics")) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = tr("basemap_light"),
                       options = providerTileOptions(attribution = "Tiles © Esri")) |>
      addLayersControl(baseGroups = c(tr("basemap_street"), tr("basemap_satellite"), tr("basemap_light")),
                       options = layersControlOptions(collapsed = TRUE), position = "topright")
  }

  build_vulnerability_map <- function(variable, scale_values) {
    health <- filtered_data()
    barrios <- city_barrios()
    req(nrow(health) > 0)

    values <- health[[variable]]
    color_scale <- risk_colors(variable, scale_values)
    bounds <- st_bbox(health)
    selected <- barrios |> filter(barrio_id %in% isolate(selected_barrio_id()))

    map <- leaflet(health) |>   # giving leaflet the data also sets a sensible starting view
      add_basemaps() |>
      # Highlight outlines get their own pane above the barrio outlines, so a
      # hovered barrio (bringToFront) can't hide the selected one.
      addMapPane("pane_highlight", zIndex = 450) |>
      # Layer 1: health polygons, filled, no outline. Not interactive - the
      # barrio outlines above catch the mouse, and we work out which health
      # polygon was clicked from the click position (see select_at() below).
      addPolygons(
        data = health,
        fillColor = color_scale(values),
        fillOpacity = 0.85,
        stroke = FALSE,
        options = pathOptions(interactive = FALSE)
      ) |>
      # Layer 2: barrio outlines on top. Hover shows the barrio NAME only.
      addPolygons(
        data = barrios,
        layerId = barrios$barrio_id,
        label = barrios$barrio_name,
        fill = TRUE, fillOpacity = 0,   # transparent, but still catches hover/click
        color = "#222222", weight = 0.8, opacity = 0.9,
        highlightOptions = highlightOptions(weight = 2.5, color = "#000000", bringToFront = TRUE)
      ) |>
      fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])

    # Keep the clicked barrio outlined when the map is rebuilt (e.g. switching
    # to Split view); later clicks swap it via the observer further down.
    if (nrow(selected) > 0) {
      map <- map |> add_highlight_outline(selected, group = "selected_outline", pane = "pane_highlight")
    }
    map
  }

  is_split <- reactive(identical(input$edf_view, "split"))

  # Which single variable the (one) map shows when not in split view.
  single_var <- reactive(if (identical(input$edf_view, "disparity")) "edf_disparity" else "edf")

  # Titles above the map(s). (The map widgets themselves live in ui.R and are
  # never rebuilt - see the note there.)
  output$explorer_title1 <- renderUI(tags$strong(var_label(if (is_split()) "edf_dE" else single_var(), tr)))
  output$explorer_title2 <- renderUI(tags$strong(var_label("edf_dT", tr)))

  # Left (or only) map.
  output$explorer_map <- renderLeaflet({
    health <- filtered_data()
    if (is_split()) {
      build_vulnerability_map("edf_dE", c(health$edf_dE, health$edf_dT))
    } else {
      build_vulnerability_map(single_var(), health[[single_var()]])
    }
  })

  # Right map - only drawn in split view. Carries the shared legend.
  output$explorer_map2 <- renderLeaflet({
    req(is_split())
    health <- filtered_data()
    build_vulnerability_map("edf_dT", c(health$edf_dE, health$edf_dT))
  })

  # One horizontal legend below the map(s). Left = lower risk, right = the
  # highest-risk (darkest) end. In split view it is shared by both maps.
  # A vertical line on a legend bar at the selected barrio's value (nothing
  # when no barrio is selected). The bar runs from range_values[1] on the left
  # to range_values[2] on the right; the social inclusion bar has 5 equal
  # blocks, and the line sits in the middle of the selected category's block.
  # Hovering the line shows the value.
  value_marker <- function(variable, range_values = NULL, column = variable) {
    row <- selected_polygon()
    if (is.null(row) || nrow(row) != 1 || is.na(row[[column]])) return(NULL)
    value <- row[[column]]
    if (variable %in% categorical_vars) {
      position <- (as.integer(value) - 0.5) / 5
      value_text <- deprivation_label(value, tr)
    } else {
      position <- if (diff(range_values) == 0) 0.5 else (value - range_values[1]) / diff(range_values)
      value_text <- format_value(variable, value, lang)
    }
    tags$span(class = "legend-marker", title = paste0(var_label(column, tr), ": ", value_text),
              style = sprintf("left: %.2f%%; background: %s;", 100 * min(max(position, 0), 1), highlight_color))
  }

  output$explorer_legend <- renderUI({
    health <- filtered_data()
    req(nrow(health) > 0)

    variable <- if (is_split()) "edf_dE" else single_var()
    values <- if (is_split()) c(health$edf_dE, health$edf_dT) else health[[single_var()]]
    title <- if (is_split()) tr("legend_split_title") else var_label(single_var(), tr)

    range_values <- range(values, na.rm = TRUE)
    colors <- risk_colors(variable, values)(seq(range_values[1], range_values[2], length.out = 8))
    tick_values <- seq(range_values[1], range_values[2], length.out = 5)
    # Split view: one line per map (both share this scale).
    marker_columns <- if (is_split()) c("edf_dE", "edf_dT") else single_var()

    tags$div(
      style = "max-width: 480px; flex-shrink: 0; margin-top: 4px;",
      tags$div(class = "small fw-bold", title),
      tags$div(class = "legend-bar",
               style = sprintf("height: 12px; border-radius: 2px; background: linear-gradient(to right, %s);",
                               paste(colors, collapse = ", ")),
               lapply(marker_columns, function(column) value_marker(variable, range_values, column))),
      tags$div(class = "d-flex justify-content-between small text-muted",
               lapply(tick_values, function(x) tags$span(format_value(variable, x, lang))))
    )
  })

  # ---- Selection and hover state
  selected_zone_id   <- reactiveVal(NULL)  # L2_5LOCALID of the selected health polygon
  selected_barrio_id <- reactiveVal(NULL)  # barrio under the last click
  hovered_barrio_id  <- reactiveVal(NULL)  # barrio under the mouse right now

  # Given a clicked map position, find the barrio under it and select the
  # health polygon that covers most of that barrio (majority_health_id, from
  # prepare_data.R) - not the health polygon directly under the click.
  # Outside every barrio, fall back to the health polygon under the click.
  select_at <- function(lat, lng) {
    point <- st_sfc(st_point(c(lng, lat)), crs = 4326)

    barrios <- city_barrios()
    barrio_hit <- which(st_intersects(barrios, point, sparse = FALSE)[, 1])
    new_barrio <- if (length(barrio_hit) > 0) barrios$barrio_id[barrio_hit[1]] else NULL

    if (!is.null(new_barrio)) {
      new_zone <- barrios$majority_health_id[barrio_hit[1]]
    } else {
      health <- filtered_data()
      health_hit <- which(st_intersects(health, point, sparse = FALSE)[, 1])
      new_zone <- if (length(health_hit) > 0) health$L2_5LOCALID[health_hit[1]] else NULL
    }

    # Clicking the same barrio / health polygon again clears the selection.
    if (identical(new_zone, selected_zone_id()) && identical(new_barrio, selected_barrio_id())) {
      new_zone <- NULL
      new_barrio <- NULL
    }
    selected_zone_id(new_zone)
    selected_barrio_id(new_barrio)
  }

  # Both maps drive the same selection / hover state. (The map-level click
  # event also fires for clicks on the barrio outlines, so it is the only
  # one we listen to - otherwise one click would be counted twice.)
  for (map_id in c("explorer_map", "explorer_map2")) {
    local({
      id <- map_id
      observeEvent(input[[paste0(id, "_click")]], {
        click <- input[[paste0(id, "_click")]]
        select_at(click$lat, click$lng)
      })
      observeEvent(input[[paste0(id, "_shape_mouseover")]], {
        hovered_barrio_id(input[[paste0(id, "_shape_mouseover")]]$id)
      })
      observeEvent(input[[paste0(id, "_shape_mouseout")]], {
        hovered_barrio_id(NULL)
      })
    })
  }

  # Nothing is selected to begin with, and the selection is cleared whenever
  # the city or age group changes.
  observeEvent(filtered_data(), {
    selected_zone_id(NULL)
    selected_barrio_id(NULL)
    hovered_barrio_id(NULL)
  })

  observeEvent(input$clear_selection, {
    selected_zone_id(NULL)
    selected_barrio_id(NULL)
  })

  # The barrio to outline on the small maps: the clicked one, which stays put
  # until another barrio is clicked or the selection is cleared. With nothing
  # selected, the hovered one. (debounce = wait 100 ms so the small maps don't
  # redraw on every pixel of mouse movement.)
  highlighted_barrio_id <- debounce(reactive({
    selected <- selected_barrio_id()
    if (!is.null(selected)) selected else hovered_barrio_id()
  }), 100)

  # The selected health polygon's row, or NULL when nothing is selected.
  selected_polygon <- reactive({
    if (is.null(selected_zone_id())) return(NULL)
    filtered_data() |> st_drop_geometry() |> filter(L2_5LOCALID == selected_zone_id())
  })

  # In split view, hovering a barrio on one map outlines the same barrio
  # on BOTH maps (each map also has its own black hover outline).
  observe({
    hovered <- hovered_barrio_id()
    barrios <- city_barrios()
    highlight <- barrios |> filter(barrio_id %in% hovered)
    for (map_id in c("explorer_map", "explorer_map2")) {
      proxy <- leafletProxy(map_id) |> clearGroup("hover_outline")
      if (is_split() && nrow(highlight) > 0) {
        proxy |> add_highlight_outline(highlight, group = "hover_outline", pane = "pane_highlight")
      }
    }
  })

  # The clicked barrio stays outlined on the main map(s) until another barrio
  # is clicked or the selection is cleared.
  observe({
    selected <- selected_barrio_id()
    barrios <- city_barrios()
    highlight <- barrios |> filter(barrio_id %in% selected)
    for (map_id in c("explorer_map", "explorer_map2")) {
      proxy <- leafletProxy(map_id) |> clearGroup("selected_outline")
      if (nrow(highlight) > 0) {
        proxy |> add_highlight_outline(highlight, group = "selected_outline", pane = "pane_highlight")
      }
    }
  })

  # ---- Relative-risk curves (data/curves.csv.gz - see archive/curves/README.md)
  # Light grey = every health polygon in the city / age group, dark grey = the
  # city-average curve, blue + shaded 95% interval = the selected polygon.
  city_curve_color <- "#404040"   # dark grey
  output$rr_curve <- renderPlot({
    req(input$city, input$age_group)
    curves <- curves_df |> filter(city == input$city, age_group == input$age_group)
    req(nrow(curves) > 0)

    polygon_curves <- curves |> filter(L2_5LOCALID != "city")
    city_curve     <- curves |> filter(L2_5LOCALID == "city")
    selected_id    <- as.character(selected_zone_id())

    # Dashed reference lines at the city-wide temperature percentiles the
    # model is built around (rr = 1 at the 75th percentile).
    reference_temps <- city_curve |>
      filter(percentile %in% c(75, 90, 95, 99)) |>
      mutate(label = vapply(percentile, function(p) tr("curve_percentile", percentile = p), ""),
             color = c("#7f7f7f", "#ff7f0e", "#9467bd", "#8c564b")[match(percentile, c(75, 90, 95, 99))],
             label_row = c(1.1, 2.4, 1.1, 2.4)[match(percentile, c(75, 90, 95, 99))])  # stagger: P90 / P95 sit close

    p <- ggplot(polygon_curves, aes(x = temperature, y = rr, group = L2_5LOCALID)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      geom_vline(data = reference_temps, aes(xintercept = temperature), color = reference_temps$color,
                 linetype = "dashed", alpha = 0.7) +
      # Percentile labels centred on their lines, on a white background so the
      # dashed line doesn't run through the text.
      geom_label(data = reference_temps, aes(x = temperature, y = Inf, label = label), inherit.aes = FALSE,
                 color = reference_temps$color, fill = "white", linewidth = 0, size = 3,
                 label.padding = unit(0.1, "lines"), vjust = reference_temps$label_row, hjust = 0.5) +
      geom_line(color = "grey70", alpha = 0.5, linewidth = 0.3) +
      geom_line(data = city_curve, color = city_curve_color, linewidth = 1.2)

    # The selected curve is added last so it sits on top of everything.
    if (length(selected_id) == 1) {
      selected_curve <- polygon_curves |> filter(L2_5LOCALID == selected_id)
      p <- p +
        geom_ribbon(data = selected_curve, aes(x = temperature, ymin = rr_lci, ymax = rr_uci), inherit.aes = FALSE,
                    fill = highlight_color, alpha = 0.1) +   # 90% transparent
        geom_line(data = selected_curve, color = highlight_color, linewidth = 1.6)
    }

    # Keep the y-axis fitted to the curves themselves (a wide interval band is
    # clipped instead of squashing every line), plus headroom at the top for
    # the percentile labels.
    y_range <- range(curves$rr, na.rm = TRUE)
    marks <- number_marks(lang)   # decimal comma in Portuguese
    axis_numbers <- label_number(decimal.mark = marks$decimal, big.mark = marks$big)
    p +
      coord_cartesian(ylim = c(y_range[1], y_range[2] + 0.25 * diff(y_range))) +
      scale_x_continuous(labels = axis_numbers) +
      scale_y_continuous(labels = axis_numbers) +
      labs(x = tr("curve_x"), y = tr("curve_y")) +
      theme_minimal(base_size = 13)
  })

  # Small legend to the right of the curve chart, one entry per line. The
  # selected entry only appears when something is selected, and names the
  # clicked barrio.
  output$rr_legend <- renderUI({
    legend_item <- function(color, height, ...) {
      tags$div(
        class = "d-flex mb-2", style = "line-height: 1.25;",
        # swatch centred on the first line of text, whatever its height
        tags$span(style = sprintf("flex: 0 0 16px; height: %spx; background: %s; margin: calc(0.625em - %spx) 6px 0 0;",
                                  height, color, height / 2)),
        tags$div(...)
      )
    }
    selected_items <- NULL
    if (!is.null(selected_zone_id())) {
      barrios <- city_barrios()
      name <- barrios$barrio_name[match(selected_barrio_id(), barrios$barrio_id)]
      if (length(name) == 0 || is.na(name)) name <- tr("curve_selected_area")
      selected_items <- list(
        legend_item(highlight_color, 3, tags$strong(name)),
        # the shaded band: a taller swatch in the band's colour and opacity
        legend_item(scales::alpha(highlight_color, 0.25), 10, tr("curve_ci"))
      )
    }
    tags$div(
      class = "small",
      selected_items,
      legend_item(city_curve_color, 3, tr("curve_city_average")),
      legend_item("#b3b3b3", 1, tr("curve_neighborhoods"))
    )
  })

  # ---- Detail box. Title = the barrio that was clicked, or "City-wide average"
  # when nothing is selected.
  output$explorer_detail_title <- renderUI({
    if (is.null(selected_zone_id())) return(tr("detail_city_wide"))
    barrios <- city_barrios()
    name <- barrios$barrio_name[match(selected_barrio_id(), barrios$barrio_id)]
    if (length(name) == 0 || is.na(name)) name <- tr("detail_no_barrio")
    # the name with a "Clear selection" link right next to it, and a note on
    # what the bracketed numbers below are compared to
    tags$div(
      tags$span(name, actionLink("clear_selection", tr("detail_clear"), class = "ms-3 small")),
      tags$div(class = "small text-muted", tr("detail_comparison_note"))
    )
  })

  output$explorer_detail <- renderUI({
    all_polygons <- filtered_data() |> st_drop_geometry()
    req(nrow(all_polygons) > 0)
    row <- selected_polygon()   # NULL = nothing selected -> show city-wide averages

    # Each line: which column, its label, scaling to display units, decimals, unit.
    # EDF line(s) first (full width), then the six small-map variables in the
    # same order as the small maps (two columns).
    edf_columns <- if (is_split()) c("edf_dE", "edf_dT") else single_var()
    edf_lines <- lapply(edf_columns, function(column) list(column = column, scale = 100, digits = 2, unit = "%"))
    map_lines <- list(
      list(column = "mean_temp",          scale = 1, digits = 1, unit = "°C"),
      list(column = "deprivation_group"),   # a category, handled separately
      list(column = "population_density", scale = 1, digits = 0, unit = "/ha"),
      list(column = "perc_informal",      scale = 1, digits = 1, unit = "%"),
      list(column = "perc_imperv",        scale = 1, digits = 1, unit = "%"),
      list(column = "perc_tree",          scale = 1, digits = 1, unit = "%")
    )

    format_number <- function(x, line) paste0(format_num(x * line$scale, line$digits, lang), line$unit)

    # Social inclusion is a 1-5 category, so instead of an average and a
    # difference it shows just the category - and nothing city-wide.
    category_stats <- function(column) deprivation_label(row[[column]], tr)
    if (is.null(row)) map_lines <- Filter(function(line) !line$column %in% categorical_vars, map_lines)

    # Each variable: "Label:" on one line, the numbers on the line below.
    make_line <- function(line) {
      stats <- if (line$column %in% categorical_vars) {
        category_stats(line$column)
      } else {
        average <- mean(all_polygons[[line$column]], na.rm = TRUE)
        if (is.null(row)) {
          format_number(average, line)
        } else {
          # Difference from the city average in words, e.g. "(0.3 below)".
          # The header says what the brackets are compared to.
          difference <- round((row[[line$column]] - average) * line$scale, line$digits)
          comparison <- if (difference == 0) tr("detail_same") else
            tr(if (difference > 0) "detail_above" else "detail_below", difference = format_num(abs(difference), line$digits, lang))
          sprintf("%s (%s)", format_number(row[[line$column]], line), comparison)
        }
      }
      tags$p(class = "mb-2", tags$strong(paste0(var_label(line$column, tr), ":")), tags$br(), stats)   # mb-2 = tighter spacing between variables
    }

    # One wrapper, so the card's 16px gap isn't added between every variable.
    # Two columns for the map variables, and for the two EDF lines in split view.
    two_columns <- "display: grid; grid-template-columns: 1fr 1fr; column-gap: 12px;"
    tags$div(
      tags$div(style = if (is_split()) two_columns, lapply(edf_lines, make_line)),
      tags$div(style = two_columns, lapply(map_lines, make_line))
    )
  })

  # ---- Small reference maps: health polygons (no outline) + barrio outlines.
  # They are drawn ONCE per city / age group. Hovering or selecting a barrio
  # only swaps a highlight outline on top (see the observer below), which is fast
  # and doesn't redraw the maps.
  mini_map_ids <- c(mini_map_temp = "mean_temp", mini_map_deprivation = "deprivation_group",
                    mini_map_imperv = "perc_imperv", mini_map_tree = "perc_tree",
                    mini_map_informal = "perc_informal", mini_map_density = "population_density")

  # A small horizontal legend shown BELOW each map. Values run low -> high from
  # left to right; the darkest colour is always the highest-risk end (so for
  # tree cover the dark end is on the left, at the low values).
  mini_legend <- function(variable, values) {
    range_values <- NULL
    if (variable %in% categorical_vars) {
      colors <- risk_colors(variable)(deprivation_levels)   # low -> high
      stops <- paste(sprintf("%s %d%% %d%%", colors, (0:4) * 20, (1:5) * 20), collapse = ", ")
      low_label <- deprivation_label(1, tr); high_label <- deprivation_label(5, tr)
    } else {
      range_values <- range(values, na.rm = TRUE)
      steps <- seq(range_values[1], range_values[2], length.out = 6)
      stops <- paste(risk_colors(variable, values)(steps), collapse = ", ")
      low_label <- format_value(variable, steps[1], lang); high_label <- format_value(variable, steps[6], lang)
    }
    tags$div(
      class = "small", style = "flex-shrink: 0; padding: 0 0 1px;",
      tags$div(class = "legend-bar",
               style = sprintf("height: 10px; border-radius: 2px; background: linear-gradient(to right, %s);", stops),
               value_marker(variable, range_values)),
      tags$div(class = "d-flex justify-content-between text-muted", style = "line-height: 1.1;", tags$span(low_label), tags$span(high_label))
    )
  }

  make_mini_map <- function(variable) {
    health <- filtered_data()
    barrios <- city_barrios()
    req(nrow(health) > 0)

    values <- health[[variable]]
    color_scale <- risk_colors(variable, values)
    highlight <- barrios |> filter(barrio_id %in% isolate(highlighted_barrio_id()))
    bounds <- st_bbox(health)

    map <- leaflet(health, options = leafletOptions(
      zoomSnap = 0, zoomControl = FALSE, dragging = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE,
      boxZoom = FALSE, touchZoom = FALSE, keyboard = FALSE, attributionControl = FALSE)) |>
      addPolygons(data = health, fillColor = color_scale(if (variable %in% categorical_vars) as.character(values) else values), fillOpacity = 1, stroke = FALSE,
                  options = pathOptions(interactive = FALSE)) |>
      addPolygons(data = barrios, fill = FALSE, color = "#444444", weight = 0.4, opacity = 0.8,
                  options = pathOptions(interactive = FALSE)) |>
      # zoomSnap = 0 above lets the map zoom by fractions, so the city fills the box
      fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]]) |>
      # The box can change size after the map is drawn (page layout settling,
      # window resize), so refit to the city every time the box is resized.
      htmlwidgets::onRender("
        function(el, x, b) {
          var map = this;
          var bounds = [[b[1], b[0]], [b[3], b[2]]];
          function refit() {
            map.invalidateSize(false);
            map.fitBounds(bounds, {animate: false});
            if (map._afterRefit) map._afterRefit();   // linked zoom: re-apply the main map's zoom
          }
          map._refit = refit;   // lets the page reset this map to the city view (linked zoom)
          refit();
          if (window.ResizeObserver) new ResizeObserver(refit).observe(el);
        }
      ", data = unname(as.numeric(bounds)))

    if (nrow(highlight) > 0) {
      map <- map |> add_highlight_outline(highlight, group = "highlight", weight = 1.5)
    }
    map
  }

  for (output_id in names(mini_map_ids)) {
    local({
      id <- output_id
      variable <- mini_map_ids[[id]]
      output[[id]] <- renderLeaflet(make_mini_map(variable))
      output[[paste0(id, "_legend")]] <- renderUI(mini_legend(variable, filtered_data()[[variable]]))
    })
  }

  # Swap the highlight outline when the hovered / selected barrio changes.
  observe({
    barrios <- city_barrios()
    highlight <- barrios |> filter(barrio_id %in% highlighted_barrio_id())
    for (id in names(mini_map_ids)) {
      proxy <- leafletProxy(id) |> clearGroup("highlight")
      if (nrow(highlight) > 0) {
        proxy |> add_highlight_outline(highlight, group = "highlight", weight = 1.5)
      }
    }
  })


  # --------------------------------------------------------------------
  # 3. DATA EXPLORER TAB
  # --------------------------------------------------------------------
  # One map, one layer per health variable plus the barrio outlines. Each
  # layer lives in its own Leaflet "pane", and the little JavaScript helper
  # in ui.R changes that pane's visibility / opacity / stacking order.

  # ---- The Data Explorer's own city / age-group selection (dx_city /
  # dx_age_group), independent of the Heat-Health Mapping tab.
  output$dx_age_group_ui <- renderUI({
    selectInput("dx_age_group", tr("label_age_group"), choices = age_group_choices(input$dx_city, tr), selected = "all")
  })

  dx_city_name <- reactive(input$dx_city)
  dx_age_name  <- reactive(input$dx_age_group)

  dx_data <- reactive({
    req(dx_city_name(), dx_age_name())
    health_sf |> filter(city == dx_city_name(), age_group == dx_age_name())
  })
  dx_barrios <- reactive({
    req(dx_city_name())
    barrios_sf |> filter(city == dx_city_name())
  })

  layer_ids <- c("barrio_outlines", layer_vars)
  layer_label <- function(id) if (id == "barrio_outlines") tr("dx_barrio_outlines") else var_label(id, tr)

  # Starting state (edit here to change what is on when the tab first opens).
  layer_order   <- reactiveVal(layer_ids)   # first item = drawn on top
  layer_show    <- reactiveVal(setNames(as.list(layer_ids %in% c("barrio_outlines", "edf")), layer_ids))
  layer_opacity <- reactiveVal(setNames(as.list(ifelse(layer_ids == "barrio_outlines", 1, 0.85)), layer_ids))

  # Color function for a layer (same one is used on the map and in the layer list).
  layer_colors <- function(id, values) risk_colors(id, values)

  # ---- The map itself (drawn once per city / age group)
  output$dx_map <- renderLeaflet({
    health <- dx_data()
    barrios <- dx_barrios()
    req(nrow(health) > 0)
    bounds <- st_bbox(health)

    map <- leaflet(health) |> add_basemaps()

    # One pane + one polygon layer per health variable (no outlines).
    for (id in layer_vars) {
      pane_name <- paste0("pane_", id)
      color_fun <- layer_colors(id, health[[id]])
      map <- map |>
        addMapPane(pane_name, zIndex = 401) |>
        addPolygons(data = health, fillColor = color_fun(health[[id]]), fillOpacity = 1, stroke = FALSE,
                    options = pathOptions(pane = pane_name, interactive = FALSE))
    }

    # Barrio outlines in their own pane. Hover shows the barrio name.
    map |>
      addMapPane("pane_barrio_outlines", zIndex = 401) |>
      addPolygons(data = barrios, label = barrios$barrio_name,
                  fill = TRUE, fillOpacity = 0, color = "#222222", weight = 0.8, opacity = 0.9,
                  highlightOptions = highlightOptions(weight = 2.5, color = "#000000", bringToFront = TRUE),
                  options = pathOptions(pane = "pane_barrio_outlines")) |>
      fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])
  })

  # ---- Send visibility / opacity / stacking order to the browser
  observe({
    req(input$main_tabs == "dx")
    dx_data()  # re-send after the map is redrawn for a new city / age group

    order <- layer_order()
    show <- layer_show()
    opacity <- layer_opacity()

    styles <- lapply(seq_along(order), function(i) {
      id <- order[i]
      list(pane = paste0("pane_", id),
           z = 401 + length(order) - i,   # earlier in the list = higher z = on top
           show = isTRUE(show[[id]]),
           opacity = opacity[[id]])
    })
    session$sendCustomMessage("layer_style", styles)
  })

  # ---- The layer list. Each row: drag handle, checkbox + name, histogram
  # button, small opacity slider, and the colour key underneath.
  output$dx_controls <- renderUI({
    health <- dx_data()
    req(nrow(health) > 0)

    order <- isolate(layer_order())     # isolate: dragging must not rebuild the list
    show <- isolate(layer_show())
    opacity <- isolate(layer_opacity())

    # A thin colour bar with the layer's lowest / highest values at the ends.
    # The darkest colour is always the highest-risk end.
    color_key <- function(id) {
      if (id == "barrio_outlines") return(NULL)   # outlines have no colour scale

      if (id %in% categorical_vars) {
        colors <- layer_colors(id, NULL)(deprivation_levels)
        stops <- paste(sprintf("%s %d%% %d%%", colors, (0:4) * 20, (1:5) * 20), collapse = ", ")
        low_label <- deprivation_label(1, tr); high_label <- deprivation_label(5, tr)
      } else {
        values <- health[[id]]
        range_values <- range(values, na.rm = TRUE)
        steps <- seq(range_values[1], range_values[2], length.out = 6)
        stops <- paste(layer_colors(id, values)(steps), collapse = ", ")
        low_label <- format_value(id, steps[1], lang); high_label <- format_value(id, steps[6], lang)
      }
      tags$div(
        class = "dx-key",
        tags$div(style = sprintf("height: 8px; border-radius: 4px; background: linear-gradient(to right, %s);", stops)),
        tags$div(class = "d-flex justify-content-between text-muted", tags$span(low_label), tags$span(high_label))
      )
    }

    rows <- lapply(order, function(id) {
      tags$div(
        class = "dx-row", `data-id` = id,
        tags$span(class = "dx-handle", title = tr("dx_drag"), "⠿"),
        tags$div(class = "dx-name", title = layer_label(id),
                 checkboxInput(paste0("show_", id), layer_label(id), value = isTRUE(show[[id]]))),
        if (id != "barrio_outlines") actionLink(paste0("hist_", id), icon("chart-column"), class = "dx-hist", title = tr("dx_hist_tooltip")),
        tags$span(class = "dx-toggle", title = tr("dx_toggle_tooltip"),
                  onclick = "this.closest('.dx-row').classList.toggle('open')", "▸"),
        # Hidden until the arrow is clicked:
        tags$div(class = "dx-detail",
                 color_key(id),
                 sliderInput(paste0("opacity_", id), NULL, min = 0, max = 1, value = opacity[[id]],
                             step = 0.05, ticks = FALSE, width = "100%"),
                 tags$div(style = "margin-top: 6px;", tr("dx_opacity")))
      )
    })
    tags$div(id = "dx_list", do.call(tagList, rows))
  })

  # Dragging a row tells us the new order (top of the list = drawn on top).
  observeEvent(input$dx_order, layer_order(input$dx_order))

  # ---- Histogram popup for a layer
  hist_variable <- reactiveVal(NULL)

  output$hist_plot <- renderPlot({
    variable <- hist_variable()
    req(variable)
    health <- dx_data() |> st_drop_geometry()
    values <- health[[variable]]

    if (variable %in% categorical_vars) {
      counts <- as.data.frame(table(values), stringsAsFactors = FALSE)
      p <- ggplot(counts, aes(x = values, y = Freq)) + geom_col(fill = "#8c2981") +
        scale_x_discrete(labels = function(x) deprivation_label(x, tr)) +
        labs(x = NULL, y = tr("hist_y"))
    } else {
      p <- ggplot(data.frame(x = values), aes(x = x)) +
        geom_histogram(bins = 20, fill = "#8c2981", color = "white", na.rm = TRUE) +
        scale_x_continuous(labels = function(x) format_value(variable, x, lang)) +
        labs(x = NULL, y = tr("hist_y"))
    }
    subtitle <- tr("hist_subtitle", city = dx_city_name(), age_group = age_group_label(dx_age_name(), tr))
    p + labs(subtitle = subtitle) + theme_minimal(base_size = 13)
  })

  # ---- React to each layer's controls
  # (local() gives every layer its own copy of `id` inside the loop.)
  for (id in layer_ids) {
    local({
      layer <- id

      observeEvent(input[[paste0("show_", layer)]], {
        state <- layer_show()
        state[[layer]] <- input[[paste0("show_", layer)]]
        layer_show(state)
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("opacity_", layer)]], {
        state <- layer_opacity()
        state[[layer]] <- input[[paste0("opacity_", layer)]]
        layer_opacity(state)
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("hist_", layer)]], {
        hist_variable(layer)
        showModal(modalDialog(title = tr("hist_title", layer = layer_label(layer)),
                              plotOutput("hist_plot", height = 320),
                              easyClose = TRUE, footer = modalButton(tr("modal_close")), size = "m"))
      }, ignoreInit = TRUE)
    })
  }
}
