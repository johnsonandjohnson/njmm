# Breaks on server without this
if (Sys.info()["sysname"] == "Linux")  source("global.R")

server <- function(session, input, output) {
  # Logging
  track_usage(storage_mode = store_sqlite(path = "logs/"))

  # Sets the items in the demography select box based off either MMRatio or MMRate
  demography_select_box <- reactive({
    switch(input$mm_select_box,
      "mmrate_per_1kyl" = UI_GLOBALS$MM_DEMO_CHOICES_MMRATE,
      "mmratio_per_100klb" = UI_GLOBALS$MM_DEMO_CHOICES_MMRATIO
    )
  })

  # Demography select box
  output$demography_select_box <- renderUI({
    radioButtons("demography_select_box",
      label = h5(UI_GLOBALS$MM_DEMO_LABEL),
      choices = demography_select_box(),
      selected = UI_GLOBALS$MM_DEMO_DEFAULT
    )
  })

  # Sets the items in the age select box based off either MMRatio or MMRate
  age_select_box <- reactive({
    switch(input$mm_select_box,
      "mmrate_per_1kyl" = UI_GLOBALS$MM_AGE_CHOICES_MMRATE,
      "mmratio_per_100klb" = UI_GLOBALS$MM_AGE_CHOICES_MMRATIO
    )
  })

  # If age is selected in the demography select box, renders the age select box
  output$age_select_box <- renderUI({
    selectInput("age_select_box",
      label = h5("Age of Mother"),
      choices = age_select_box(),
      selected = "all_ages"
    )
  })

  # Sets the items in the ethnicity select box with all unique options and adds 'All Ethnicities'
  ethnicity_select_box <- reactive({
    UI_GLOBALS$MM_ETHNICITY_CHOICES_MMRATIO
  })

  # If ethnicity is selected in the demography select box, renders the ethnicity select box
  output$ethnicity_select_box <- renderUI({
    selectInput("ethnicity_select_box",
      label = h5("Race/Ethnicity"),
      choices = ethnicity_select_box(),
      selected = "all_ethnicities"
    )
  })

  # Map title showing MMRatio or MMRate by Year, All Women, an Age Group or Ethnicity
  map_title <- reactive({
    req(input$demography_select_box)
    dem_group_selected <- if (input$demography_select_box == "age") input$age_select_box else if (input$demography_select_box == "ethnicity") input$ethnicity_select_box

    build_map_title(input$mm_slide_year, input$mm_select_box, input$demography_select_box, dem_group_selected)
  })

  # Other layer map title
  # Reference - https://github.com/rstudio/leaflet/issues/215
  layer_map_title <- reactive({
    build_layer_map_title(input$mm_slide_year, input$other_layer_box)
  })

  # Text upon hovering on counties based on either MMRatio or MMRate
  labels_change <- reactive({

    # either mmrate or mmratio based on selection
    rate_or_ratio <- rel_mmr_df() %>% filter(year == input$mm_slide_year)
    mmr_merge <- COUNTIES@data %>%
      merge(rate_or_ratio, by = "County_Name", sort = FALSE)

    build_map_label(mmr_merge, input$mm_select_box)
  })

  # Legend range to remain constant through different years
  legend_range <- reactive({
    my_sel <- input$mm_select_box
    mmr_df <- rel_mmr_df()
    req(nrow(mmr_df) > 0)

    max_chosen <- mmr_df %>%
      pull(my_sel) %>%
      max(na.rm = TRUE)
    c(0, max_chosen)
  })

  # Generate custom color palette
  custom_pal <- reactive({
    mmr_df <- rel_mmr_df()
    req(nrow(mmr_df) > 0)

    mmr_vals <- mmr_df %>%
      pull(input$mm_select_box) %>%
      na.omit()
    req(length(mmr_vals) > 0)

    create_custom_pal(
      mmr_vals,
      input$mm_select_box
    )
  })

  # Color scheme for other layer map
  color_scheme_layer_map <- reactive({
    switch(input$other_layer_box,
      "Percent Uninsured" = COLORS_UNINSURED,
      "Percent Covered by Medicaid" = COLORS_MEDICAID,
      "Difference in NTSV Rate" = COLORS_CSECTION,
      "OB/GYN Physicians Per Capita" = COLORS_OBGYN,
      "Practitioner Midwives Per Capita" = COLORS_MIDWIVES,
      "Percent Poverty" = COLORS_POVERTY
    )
  })

  # Max for other layer map
  max_layer_map <- reactive({
    switch(input$other_layer_box,
      "Percent Uninsured" = MAX_PCT_UNINSURED_ANY,
      "Percent Covered by Medicaid" = MAX_PCT_INSURED_MEDICAID,
      "Difference in NTSV Rate" = MAX_CSECTION,
      "OB/GYN Physicians Per Capita" = MAX_OBGYN,
      "Practitioner Midwives Per Capita" = MAX_MIDWIVES,
      "Percent Poverty" = MAX_POVERTY
    )
  })

  # Variable for other layer map
  var_layer_map <- reactive({
    switch(input$other_layer_box,
      "Percent Uninsured" = "pct_uninsured",
      "Percent Covered by Medicaid" = "pct_medicaid",
      "Difference in NTSV Rate" = "ntsv_goal_diff",
      "OB/GYN Physicians Per Capita" = "per_capita_obgyns",
      "Practitioner Midwives Per Capita" = "per_capita_midwives",
      "Percent Poverty" = "percent_poverty"
    )
  })

  # Palette parameters for other layer map
  pal_layer_map <- reactive({
    # This layer will have negative values and use a reversed palette 
    # Setting min_scale to negative of MAX_CSECTION in order to have
    # the middle color (white) in color scale line up with 0
    if (input$other_layer_box == "Difference in NTSV Rate") {
      min_val <- MIN_CSECTION
      min_scale <- -(MAX_CSECTION)
      pal_reverse <- TRUE
    } else {
      min_val <- 0
      min_scale <- 0
      pal_reverse <- FALSE
    }

    list(
      "min_val" = min_val,
      "min_scale" = min_scale,
      "pal_reverse" = pal_reverse
    )
  })

  # MMR DataFrame Selection/column
  rel_mmr_df <- reactive({
    req(input$ethnicity_select_box)
    req(input$age_select_box)
    dem_group_selected <- if (input$demography_select_box == "age") input$age_select_box else if (input$demography_select_box == "ethnicity") input$ethnicity_select_box

    mmr_df(input$mm_select_box == "mmratio_per_100klb", input$demography_select_box, dem_group_selected)
  })

  # Color change parameter to ~pal() makes county colors dynamic
  mm_select_change <- reactive({
    my_sel <- input$mm_select_box
    my_year <- input$mm_slide_year

    rate_or_ratio <- rel_mmr_df() %>% filter(year == my_year)
    mmr_merge <- merge(COUNTIES@data, rate_or_ratio, by = "County_Name", sort = FALSE)

    mmr_merge %>% pull(my_sel)
  })

  # Other Layer Data Reactive
  other_layer_data <- reactive({
    req(input$mm_slide_year >= 2010)

    # Filter to chosen year
    rel_data <- OTHER_LAYER_DF %>%
      filter(year == input$mm_slide_year) %>%
      select(-per_capita_obgyns)

    # For obgyns - show most recent data, without looking ahead
    rel_obgyns <- OTHER_LAYER_DF %>%
      filter(year == obgyn_year(input$mm_slide_year)) %>%
      select(FIPS_Code, per_capita_obgyns)

    # Add back obgyn data and arrange in the county display order
    rel_data %>%
      full_join(rel_obgyns, by = c("FIPS_Code")) %>%
      arrange(NJ_COUNTY_ORDER)
  })

  output$new_jersey_map <- renderLeaflet({
    basic_map_NJ() %>%
      hospital_layer()
  })

  output$layer_map <- renderLeaflet({
    basic_map_NJ() %>%
      hospital_layer()
  })

  # Observe county colors and labels change
  observe({
    pal <- custom_pal()

    leafletProxy("new_jersey_map") %>%
      clearShapes() %>%
      addPolygons(
        data = COUNTIES,
        color = "black",
        fillColor = ~ pal(mm_select_change()),
        fillOpacity = .7,
        weight = 2,
        group = "counties", # Group by "counties" in order to call layer toggle
        highlight = map_highlight(), # Option to highlight upon hover
        label = labels_change(),
        labelOptions = map_label(),
        options = pathOptions(pane = "polygons")
      )
  })

  # Observe legend change
  observe({
    pal <- custom_pal()
    leafletProxy("new_jersey_map") %>%
      clearControls() %>%
      addLegend(pal = pal, values = legend_range(), title = "MMR", opacity = 1)
  })

  # Observe contextual layer change
  observeEvent(list(input$other_layer_box, input$mm_slide_year), {
    layer_var <- other_layer_data() %>%
      pull(var_layer_map())

    pal_params <- pal_layer_map()
    color_scheme <- color_scheme_layer_map()
    max_val <- max_layer_map()

    title <- case_when(
      any(grepl("Percent|NTSV", input$other_layer_box)) ~ "%",
      !(grepl("Percent", input$other_layer_box)) ~ "#"
    )

    leafletProxy("layer_map") %>%
      clearShapes() %>%
      clearControls() %>%
      add_polygons_layer_map(
        .,
        color_scheme,
        input$other_layer_box,
        max_val,
        layer_var
      ) %>%
      addLegend(
        pal = colorNumeric(color_scheme,
          domain = c(pal_params$min_scale, max_val),
          reverse = pal_params$pal_reverse
        ),
        values = c(pal_params$min_val, max_val),
        title = title,
        opacity = 1
      )
  })

  # Reset selected value of mm_select_box to default value and year to 2017
  observeEvent(input$reset_input, {
    updateSelectInput(session, "mm_select_box", selected = "mmratio_per_100klb")
    updateSliderInput(session, "mm_slide_year", value = 2017)
    updateSelectInput(session, "demography_select_box", selected = "all")

    leafletProxy("new_jersey_map") %>% setView(lng = -74.406, lat = 40.058, zoom = 7.8)
    leafletProxy("layer_map") %>% setView(lng = -74.406, lat = 40.058, zoom = 7.8)
  })

  # Reset selected value of mm_select_box2 to default value and year to 2017
  observeEvent(input$reset_input2, {
    updateSelectInput(session, "mm_select_box2", selected = "mmratio_per_100klb")
    updateSelectInput(session, "mm_county_box", selected = MM_COUNTY_CHOICES[1])
    updateSliderInput(session, "mm_slide_year2", value = 2017)
    updateRadioButtons(session, "demography_radio", selected = "age")
  })

  # Observe rate/ratio switch to reset Demography Select Box to All Women
  observeEvent(input$mm_select_box, {
    req(input$demography_select_box)
    updateSelectInput(session, "demography_select_box", selected = "all")
  })

  output$mm_formula <- renderUI({
    switch(input$mm_select_box,
      "mmrate_per_1kyl" = UI_GLOBALS$MMRATE_LATEX,
      "mmratio_per_100klb" = UI_GLOBALS$MMRATIO_LATEX
    ) %>%
      helpText() %>%
      withMathJax()
  })

  output$mm_formula2 <- renderUI({
    switch(input$mm_select_box2,
      "mmrate_per_1kyl" = UI_GLOBALS$MMRATE_LATEX,
      "mmratio_per_100klb" = UI_GLOBALS$MMRATIO_LATEX
    ) %>%
      helpText() %>%
      withMathJax()
  })

  output$map_title <- renderUI({
    map_title()
  })

  output$layer_map_title <- renderUI({
    layer_map_title()
  })

  ### Explore Demography Tab

  # Sets the items in the demography radio options based off either MMRatio or MMRate
  demography_radio <- reactive({
    switch(input$mm_select_box2,
      "mmrate_per_1kyl" = UI_GLOBALS$MM_DEMO_RADIO_CHOICES_MMRATE,
      "mmratio_per_100klb" = UI_GLOBALS$MM_DEMO_RADIO_CHOICES_MMRATIO
    )
  })

  # Demography radio
  output$demography_radio <- renderUI({
    radioButtons("demography_radio",
      label = h5(UI_GLOBALS$MM_DEMO_LABEL),
      choices = demography_radio(),
      selected = UI_GLOBALS$MM_DEMO_RADIO_DEFAULT
    )
  })

  # MMR DataFrame Selection for non-geo tab
  rel_mmr_df2 <- reactive({
    req(input$demography_radio)
    mmr_df_explore_more(input$mm_select_box2, input$mm_slide_year2, input$demography_radio, input$mm_county_box)
  })

  # Plotly graph
  output$new_jersey_graph <- renderPlotly({
    validate(
      need(length(rel_mmr_df2()) > 1, "") # Validate that our rate or ratio is not all zeros (# of columns)
    )
    one_plot(rel_mmr_df2())
  })

  # Map title showing MMRatio or MMRate by Year, All Women, an Age Group or Ethnicity
  graph_title <- reactive({
    build_graph_title(length(rel_mmr_df2()) <= 1, input$mm_slide_year2, input$mm_select_box2, input$mm_county_box)
  })

  output$graph_title <- renderUI({
    graph_title()
  })

  # Info Box
  sel_data <- reactive({
    data <- rel_mmr_df() %>% filter(year == input$mm_slide_year)
  })

  sel_data_numerator <- reactive({
    data <- sel_data()
    numerator <- sum(data$maternal_deaths)
  })

  sel_data_denominator <- reactive({
    data <- sel_data()

    denominator <- if (input$mm_select_box == "mmrate_per_1kyl") sum(data$years_lived) else sum(data$live_births)
  })

  overall_measurement <- reactive({
    multiplier <- if (input$mm_select_box == "mmrate_per_1kyl") 1000 else 100000

    mmrate_or_ratio <- round(sel_data_numerator() / sel_data_denominator() * multiplier, 1) %>% as.character()
  })

  output$info_box <- renderUI({
    overall_measurement()
  })

  output$info_box_title <- renderUI({
    title <- paste("Overall NJ", map_title()) %>%
      str_replace(" by County", "") %>%
      HTML()
  })

  output$info_box_subtitle <- renderUI({
    ending <- if (input$mm_select_box == "mmrate_per_1kyl") "Years Lived" else "Live Births"

    subtitle <- paste(
      format(sel_data_numerator(), big.mark = ","), "Maternal Deaths", "</br>",
      format(sel_data_denominator(), big.mark = ","), ending
    ) %>%
      HTML()
  })

  output$census_tbl_url <- renderUI({
    if (input$mm_county_box != "All Counties of NJ") {
      this_fips <- counties@data %>%
        filter(County_Name == input$mm_county_box) %>%
        pull(FIPSSTCO)

      this_url <- build_census_tbl_url(input$mm_slide_year2, as.character(this_fips), geo_level = "county")
    } else {
      this_url <- build_census_tbl_url(input$mm_slide_year2, "34", geo_level = "state")
    }

    this_url %>%
      a("For reference, a detailed Race/Ethnicity breakdown is available here from the Census Bureau.",
        href = .
      )
  })


  # Footnote Modal
  observeEvent(input$footnote_modal, {
    showModal(modalDialog(
      title = "Footnote: More Details",
      HTML(paste0(
        "<ul>",
        "<li>Individuals whose delivery date and maternal death occur in different calendar years</li>",
        "<li>Individuals whose age at delivery and maternal death occur in different age groups</li>",
        "<li>Multiple births (e.g. twins)</li>",
        "</ul>"
      ))
    ))
  })
  
  # Explore More Footnote Modal
  observeEvent(input$explore_more_modal, {
    showModal(modalDialog(
      title = "Footnote: More Details",
      HTML(paste0(
        "<ul>",
        "<li>This situation can occur when it is possible to stratify the data such that one strata has 0 
        live births but 1 or more maternal deaths.</li>",
        "<li>One possible scenario in which this situation can occur is if a live birth occurs in 2013, 
        but the  associated maternal death occurs in 2014.</li>",
        "</ul>"
      ))
    ))
  })

  ## Bookmark Server Functions
  # Need to exclude the buttons from themselves being bookmarked
  # and other inputs that aren't key for state

  # URL Shortening
  onBookmarked(function(url) {
    # Adapted from https://rdrr.io/cran/carbonate/src/R/uri_functions.R
    new_url <- paste0("http://tinyurl.com/api-create.php?url=", url) %>%
      httr::GET() %>%
      httr::content()

    showBookmarkUrlModal(new_url)
  })


  setBookmarkExclude(c(
    "bookmark1", "bookmark2",
    "sidebarItemExpanded", ".shinylogs_lastInput",
    "reset_input"
  ))

  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
}
