# ============================================================================
# UI (what the app looks like)
# ============================================================================
# Layout only: the city / age-group row and the 3 tabs
#   1. Heat-Health Mapping   2. Data Explorer   3. Information
# The logic that fills them in lives in server.R.
# All text comes from data/translations.xlsx via tr("key") (see global.R).
# ============================================================================

# Title colours for the small maps, by variable group.
group_colors <- list(
  temperature = "#b8501d",   # temperature
  social      = "#5b3f8f",   # social inclusion, population density, informal settlement
  built       = "#2d7a4f"    # impervious surface, tree cover
)

# One small map: coloured title, the map, and its horizontal legend below.
mini_map_card <- function(title, output_id, color) {
  card(
    class = "mini-card",
    card_header(title, style = sprintf("background: %s; color: white;", color)),
    leafletOutput(output_id, height = "100%"),   # fills its grid cell
    uiOutput(paste0(output_id, "_legend"))
  )
}

# EN | PT links at the right of the tab bar. Each reloads the page in that
# language; the current one is shown in bold.
language_switch <- function(lang) {
  link <- function(code, label) {
    if (code == lang) tags$strong(label) else tags$a(href = paste0("?lang=", code), label)
  }
  tags$span(class = "small", link("en", "EN"), " | ", link("pt", "PT"))
}

# ---- Heat-Health Mapping tab pieces ------------------------------------------
map_card <- card(
  class = "map-card",
  fill = FALSE,   # let the card grow to fit the maps and legend
  # One map for Total EDF; two side by side for the split view (server.R).
  tags$div(
    id = "explorer_maps",
    tags$div(uiOutput("explorer_title1"), leafletOutput("explorer_map", height = 480)),
    tags$div(class = "map2wrap", uiOutput("explorer_title2"), leafletOutput("explorer_map2", height = 480))
  ),
  uiOutput("explorer_legend")
)

curve_card <- function(tr) card(
  card_header(tr("curve_title")),
  # Chart on the left, a narrow legend to its right. The row grows to fill the
  # card (the card is as tall as the stats box beside it) and the chart fills
  # the row, never shorter than the minimum height.
  tags$div(
    style = "display: flex; gap: 8px; flex: 1 1 auto; min-height: 150px;",
    tags$div(style = "flex: 1; min-width: 0;", plotOutput("rr_curve", height = "100%")),
    tags$div(style = "flex: 0 0 130px; align-self: center;", uiOutput("rr_legend"))
  )
)

detail_card <- card(
  card_header(uiOutput("explorer_detail_title")),
  uiOutput("explorer_detail", style = "font-size: 13px;")   # smaller text for the stats
)

reference_cards <- function(tr) list(
  mini_map_card(var_label("mean_temp", tr),          "mini_map_temp",        group_colors$temperature),
  mini_map_card(var_label("deprivation_group", tr),  "mini_map_deprivation", group_colors$social),
  mini_map_card(var_label("population_density", tr), "mini_map_density",    group_colors$social),
  mini_map_card(var_label("perc_informal", tr),      "mini_map_informal",    group_colors$social),
  mini_map_card(var_label("perc_imperv", tr),        "mini_map_imperv",      group_colors$built),
  mini_map_card(var_label("perc_tree", tr),          "mini_map_tree",        group_colors$built)
)

# Top row = city / age-group controls, the curve, and the stats box; bottom
# row = main map(s) on the left and the six reference maps on the right (2 rows
# of 3, stretched to the same height as the main map card).
vulnerability_panel <- function(tr) {
  # What the main map(s) show: one map (total or disparity) or the two-map split.
  edf_view_choices <- setNames(c("total", "disparity", "split"),
                               c(tr("view_total"), tr("view_disparity"), tr("view_split")))
  nav_panel(
    tr("tab_mapping"), value = "mapping",
    layout_columns(
      col_widths = c(2, 6, 4),
      tags$div(
        selectInput("city", tr("label_city"), choices = cities, selected = cities[1]),
        uiOutput("age_group_ui"),
        selectInput("edf_view", tr("label_health_outcome"), choices = edf_view_choices, selected = "total")
      ),
      curve_card(tr),
      detail_card
    ),
    layout_columns(
      col_widths = c(6, 6),
      map_card,
      tags$div(class = "ref-grid", reference_cards(tr))
    )
  )
}

# The page is built per visitor, in the language from the URL (?lang=pt).
ui <- function(request) {
  lang <- get_language(request$QUERY_STRING)
  tr <- make_tr(lang)

  page_fluid(
    title = tr("app_title"),
    lang = lang,
    theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#c1440e"),  # other looks: https://bootswatch.com

    # Small JavaScript helper for the Data Explorer map. The R code can't
    # change a map layer's opacity / stacking order / visibility without
    # redrawing (and re-zooming) the whole map, so R sends a message here
    # and we change the layer's "pane" in the browser instead.
    # (It retries for a few seconds in case the map isn't drawn yet.)
    tags$script(HTML("
      Shiny.addCustomMessageHandler('layer_style', function(layers) {
        function apply(tries) {
          var widget = HTMLWidgets.find('#dx_map');
          var map = widget && widget.getMap ? widget.getMap() : null;
          if (!map) { if (tries < 20) setTimeout(function() { apply(tries + 1); }, 250); return; }
          var missing = false;
          layers.forEach(function(layer) {
            var pane = map.getPane(layer.pane);
            if (!pane) { missing = true; return; }
            pane.style.opacity = layer.opacity;
            pane.style.zIndex = layer.z;
            pane.style.display = layer.show ? '' : 'none';
          });
          if (missing && tries < 20) setTimeout(function() { apply(tries + 1); }, 250);
        }
        apply(0);
      });

      // Keep the maps on the Heat-Health Mapping tab panning and zooming together.
      // Checks twice a second whether the maps exist and links them (a redrawn
      // map is a new object, so it gets linked again).
      var referenceMapIds = ['mini_map_temp', 'mini_map_deprivation', 'mini_map_density',
                             'mini_map_informal', 'mini_map_imperv', 'mini_map_tree'];
      function getMap(id) {
        var widget = HTMLWidgets.find('#' + id);
        return widget && widget.getMap ? widget.getMap() : null;
      }
      // Move every reference map to follow a main map: same centre, and the same
      // zoom CHANGE (each reference map starts fitted to the city, so we apply
      // the change in zoom rather than copying the zoom level).
      function moveReferenceMaps(source) {
        referenceMapIds.forEach(function(id) {
          var ref = getMap(id);
          if (!ref || ref._baseZoom === undefined) return;
          ref.setView(source.getCenter(), ref._baseZoom + (source.getZoom() - ref._mainBaseZoom), {animate: false});
        });
      }
      setInterval(function() {
        var mapA = getMap('explorer_map'), mapB = getMap('explorer_map2');
        if (!mapA) return;

        // the two split maps follow each other
        if (mapB && !(mapA._synced === mapB && mapB._synced === mapA)) {
          mapA._synced = mapB; mapB._synced = mapA;
          var busy = false;
          function follow(from, to) {
            from.on('moveend zoomend', function() {
              if (busy) return;
              busy = true;
              to.setView(from.getCenter(), from.getZoom(), {animate: false});
              busy = false;
            });
          }
          follow(mapA, mapB); follow(mapB, mapA);
          mapB.setView(mapA.getCenter(), mapA.getZoom(), {animate: false});
        }

        // reference maps follow the main map(s)
        [mapA, mapB].forEach(function(main) {
          if (main && !main._refHandler) {
            main._refHandler = true;
            main.on('moveend zoomend', function() { moveReferenceMaps(main); });
          }
        });
        referenceMapIds.forEach(function(id) {
          var ref = getMap(id);
          if (ref && ref._followsMain !== mapA) {
            // The main map was (re)built and is back at the city view, so put
            // this reference map back at the city view too before following.
            ref._afterRefit = null;
            if (ref._refit) ref._refit();
            ref._followsMain = mapA;
            ref._baseZoom = ref.getZoom();
            ref._mainBaseZoom = mapA.getZoom();
            // If this map refits itself later (e.g. the window is resized), it goes
            // back to the city view; put it back in step with the main map's zoom.
            ref._afterRefit = (function(r, main) { return function() {
              r._baseZoom = r.getZoom();
              r.setView(main.getCenter(), r._baseZoom + (main.getZoom() - r._mainBaseZoom), {animate: false});
            }; })(ref, mapA);
          }
        });
      }, 500);
    ")),

    tags$script(src = "Sortable.min.js"),   # drag-and-drop library (saved in www/)

    tags$style(HTML("
      /* Vulnerability maps: one column for Total, two side by side for Split. Both
         map widgets stay in the page permanently (rebuilding them made the left
         map lose its hover/click events); only their layout changes. */
      #explorer_maps { display: grid; gap: 0.5rem; grid-template-columns: minmax(0, 1fr); }
      #explorer_maps.split { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      #explorer_maps:not(.split) .map2wrap { display: none; }

      /* Selected barrio's value on a legend bar: a blue line (colour set in
         server.R) with a thin dark casing, the same height as the bar */
      .legend-bar { position: relative; }
      .legend-marker { position: absolute; top: 0; bottom: 0; width: 3px; margin-left: -1.5px;
                       border-radius: 1px; box-shadow: 0 0 0 1px #111; }

      /* Less padding around the main map's legend */
      .map-card .card-body { gap: 0.15rem; padding-bottom: 0.5rem; }

      /* Less padding around the reference maps and their legends */
      .mini-card .card-body { padding: 0.5rem; gap: 0.5rem; }   /* gap under the map = padding at its sides */

      /* Reference maps: white background */
      [id^=\"mini_map_\"].leaflet-container { background: white; }

      /* Reference maps in a 3 x 2 grid that fills the height of the main map card */
      .ref-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); grid-template-rows: repeat(2, minmax(0, 1fr)); gap: 0.5rem; height: 100%; }
      .ref-grid .card { min-height: 0; margin-bottom: 0; }

      /* Data Explorer layer rows: handle, name, histogram icon, expand arrow.
         The legend and opacity slider are hidden until the row is expanded. */
      .dx-row { display: flex; flex-wrap: wrap; align-items: center; padding: 4px 0; border-bottom: 1px solid #eee; background: white; }
      .dx-handle { cursor: grab; color: #999; padding: 0 8px 0 2px; font-size: 16px; user-select: none; }
      .dx-name { flex: 1; min-width: 0; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
      .dx-name label { white-space: nowrap; }
      .dx-row .shiny-input-container { margin: 0 !important; width: auto !important; }
      .dx-row .form-check, .dx-row .checkbox { margin: 0; min-height: 0; }
      .dx-hist { padding: 0 6px; color: #555; }
      .dx-toggle { cursor: pointer; color: #777; padding: 0 6px; user-select: none; transition: transform 0.1s; }
      .dx-row.open .dx-toggle { transform: rotate(90deg); }
      .dx-detail { display: none; flex-basis: 100%; padding: 2px 8px 6px 26px; font-size: 13px; }
      .dx-row.open .dx-detail { display: block; }
      .dx-detail .shiny-input-container { width: 100% !important; }
      /* Opacity slider: hide the 0 / 1 end labels and the value bubble */
      .dx-detail .irs-min, .dx-detail .irs-max, .dx-detail .irs-single,
      .dx-detail .irs-from, .dx-detail .irs-to, .dx-detail .irs-grid { display: none; }
      .dx-detail .irs { height: 26px; margin-top: -14px; }   /* pull the slider up toward the legend */
      .dx-detail .dx-key { margin-top: 8px; }                /* space between layer name and legend */
      .dx-detail .form-group { margin-bottom: 0; }
    ")),

    # Toggle the map layout when the view radio changes, and make the Data
    # Explorer layer list draggable.
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'edf_view') {
          var maps = document.getElementById('explorer_maps');
          if (maps) maps.classList.toggle('split', e.value === 'split');
        }
      });
      function initDxSortable() {
        var list = document.getElementById('dx_list');
        if (!list || list._sortable) return;
        list._sortable = Sortable.create(list, {
          handle: '.dx-handle', animation: 120,
          onEnd: function() {
            var ids = Array.from(list.children).map(function(row) { return row.getAttribute('data-id'); });
            Shiny.setInputValue('dx_order', ids, {priority: 'event'});
          }
        });
      }
      new MutationObserver(initDxSortable).observe(document.documentElement, {childList: true, subtree: true});
    ")),

    navset_tab(
      id = "main_tabs",

      vulnerability_panel(tr),

      # ---- TAB 2: Data Explorer ------------------------------------------
      nav_panel(
        tr("tab_explorer"), value = "dx",
        # This tab has its own City / Age group dropdowns.
        layout_columns(
          col_widths = c(3, 3),
          selectInput("dx_city", tr("label_city"), choices = cities, selected = cities[1]),
          uiOutput("dx_age_group_ui")
        ),
        layout_columns(
          col_widths = c(4, 8),
          card(
            height = 860,
            card_header(tr("dx_layers")),
            tags$div(style = "overflow-y: auto;", uiOutput("dx_controls"))
          ),
          card(
            height = 860,
            full_screen = TRUE,
            card_header(tr("dx_map")),
            leafletOutput("dx_map", height = 780)
          )
        )
      ),

      # ---- TAB 3: Information --------------------------------------------
      nav_panel(
        tr("tab_info"), value = "info",
        card(
          card_header(tr("info_title")),
          # The text is info_body in data/translations.xlsx.
          text_paragraphs(tr("info_body"))
        )
      ),

      # Language switch, pushed to the right of the tabs.
      nav_spacer(),
      nav_item(language_switch(lang))
    )
  )
}
