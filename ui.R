function(request) {
  shinyUI(
    tagList(
      useShinyjs(),
      ui <- dashboardPage(
        skin = "black",
        dashboardHeader(title = "New Jersey Maternal Mortality Dashboard"),
        dashboardSidebar(
          width = 170,
          sidebarMenu(
            menuItem("Interactive Map", tabName = "map", icon = icon("map")),
            menuItem("Explore More", tabName = "bars", icon = icon("chart-bar")),
            menuItem("How This Works", tabName = "methodology", icon = icon("cogs")),
            menuItem("About", tabName = "about", icon = icon("heartbeat"))
          )
        ),
        dashboardBody(
          includeCSS("www/jnj_circular/css/stylesheet.css"),
          includeCSS("www/njmm_shiny.css"),
          tabItems(
            tabItem(
              tabName = "map",
              fluidRow(
                column(
                  width = 3,
                  box(
                    sliderInput("mm_slide_year",
                      label = UI_GLOBALS$MM_YEAR_LABEL,
                      min = UI_GLOBALS$MM_YEAR_MIN, max = UI_GLOBALS$MM_YEAR_MAX,
                      value = UI_GLOBALS$MM_YEAR_DEFAULT,
                      sep = UI_GLOBALS$MM_YEAR_SEP, width = UI_GLOBALS$MM_YEAR_WIDTH,
                      ticks = TRUE
                    ),
                    bookmarkButton(id = "bookmark1", label = "Bookmark"),
                    actionButton("reset_input", "Reset", icon = icon("refresh")),
                    tippy_this(
                      elementId = "reset_input",
                      tooltip = UI_GLOBALS$RESET_TIPPY_TEXT,
                      placement = UI_GLOBALS$RESET_TIPPY_PLACEMENT
                    ),
                    width = 12, align = "center"
                  ),
                  tabBox(
                    title = "",
                    width = 12,
                    tabPanel(
                      title = div(HTML("Maternal<br>Mortality"), style = "font-size:100%"),
                      selectInput("mm_select_box",
                        label = UI_GLOBALS$MM_SELECT_LABEL,
                        choices = UI_GLOBALS$MM_SELECT_CHOICES,
                        selected = UI_GLOBALS$MM_SELECT_DEFAULT
                      ),
                      div(uiOutput("mm_formula"), style = "font-size:75%"),
                      htmlOutput("demography_select_box"),
                      conditionalPanel(
                        condition = "input.mm_select_box == 'mmratio_per_100klb' && input.demography_select_box == 'ethnicity'",
                        htmlOutput("ethnicity_select_box")
                      ),
                      conditionalPanel(
                        condition = "input.demography_select_box == 'age'",
                        htmlOutput("age_select_box"),
                      )
                    ),
                    tabPanel(
                      div(HTML("Contextual<br>Variables"), style = "font-size:100%"),
                      selectInput("other_layer_box",
                        label = h5("Select a contextual variable"),
                        choices = c(
                          "Percent Uninsured",
                          "Percent Covered by Medicaid",
                          "Difference in NTSV Rate",
                          "OB/GYN Physicians Per Capita",
                          "Practitioner Midwives Per Capita",
                          "Percent Poverty"
                        ),
                        selected = "Percent Uninsured"
                      )
                    )
                  )
                ),
                column(
                  width = 9,
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    box(leafletOutput("new_jersey_map", height = 625) %>% withSpinner(color = "#CA001B"),
                      width = "50%",
                      title = uiOutput("map_title", style = "height: 55px")
                    ),
                    box(
                      width = "50%",
                      title = uiOutput("layer_map_title", style = "height: 55px"),
                      conditionalPanel(
                        condition = "input.mm_slide_year >= 2010",
                        leafletOutput("layer_map", height = 625) %>% withSpinner(color = "#CA001B")
                      )
                    ),
                    cellArgs = list(style = "padding: 3px; padding-top:0px")
                  )
                )
              ),
              fluidRow(
                column(width = 3, " "),
                column(
                  9,
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    box(infoBox(
                      title = "",
                      subtitle = uiOutput("info_box_subtitle"),
                      uiOutput("info_box"),
                      color = "red",
                      icon = shiny::icon("notes-medical")
                    ),
                    title = uiOutput("info_box_title"),
                    width = "50%"
                    ),
                    cellArgs = list(style = "padding: 3px; padding-top:0px")
                  )
                ),
              ),
              actionLink(
                "footnote_modal",
                "Note: Due to the small number of counts in the publicly available data 
                                 used to calculate these measures of maternal mortality, the measures of 
                                 maternal mortality may be extreme (e.g. MMRatio greater than 1)."
              )
            ),
            tabItem(
              tabName = "bars",
              fluidRow(
                column(
                  4,
                  box(selectInput("mm_select_box2",
                    label = UI_GLOBALS$MM_SELECT_LABEL,
                    choices = UI_GLOBALS$MM_SELECT_CHOICES,
                    selected = UI_GLOBALS$MM_SELECT_DEFAULT
                  ),
                  selectInput("mm_county_box",
                    label = "County",
                    choices = mmUniqueCounties,
                    selected = mmUniqueCounties[1]
                  ),
                  div(uiOutput("mm_formula2"), style = "font-size:75%"),
                  uiOutput("demography_radio"),
                  sliderInput("mm_slide_year2",
                    label = UI_GLOBALS$MM_YEAR_LABEL,
                    min = UI_GLOBALS$MM_YEAR_MIN, max = UI_GLOBALS$MM_YEAR_MAX,
                    value = UI_GLOBALS$MM_YEAR_DEFAULT,
                    sep = UI_GLOBALS$MM_YEAR_SEP, width = UI_GLOBALS$MM_YEAR_WIDTH,
                    ticks = TRUE
                  ),
                  bookmarkButton(id = "bookmark2", label = "Bookmark"),
                  actionButton("reset_input2", "Reset", icon = icon("refresh")),
                  tippy_this(
                    elementId = "reset_input2",
                    tooltip = UI_GLOBALS$RESET_TIPPY_TEXT,
                    placement = UI_GLOBALS$RESET_TIPPY_PLACEMENT
                  ),
                  width = "100%",
                  title = "Input"
                  )
                ),
                column(
                  6,
                  box(plotlyOutput("new_jersey_graph") %>% withSpinner(color = "#CA001B"),
                    width = "50%", title = uiOutput("graph_title")
                  ),
                  conditionalPanel(
                    "input.demography_radio == 'ethnicity' & input.mm_slide_year2 >= 2010",
                    box(
                      title = div(icon("info-circle"), "More Info"),
                      uiOutput("census_tbl_url"),
                      width = "100%",
                      solidHeader = TRUE, status = "danger"
                    )
                  )
                )
              ),
              actionLink(
                "explore_more_modal",
                "Note: In some cases, we may see MMRatios of all 0 when stratifying by one demographic 
                variable, but have some nonzero MMRatios when stratifying by a different demographic 
                variable."
              )
            ),
            tabItem(
              tabName = "methodology",
              h3("Maternal Mortality Ratio (MMRatio)"),
              p("The MMRatio is the number of maternal deaths divided by the number of live births."),
              UI_GLOBALS$MMRATIO_LATEX %>% withMathJax(),
              p("It is also commonly represented as the MMRatio per 100,000 live births which is the
                   previous quantity multiplied by 100,000."),
              br(),
              h3("Maternal Mortality Rate (MMRate)"),
              p("The MMRate is the number of maternal deaths divided by the number of woman-years 
                   lived between 15 and 49. Woman-years lived is the number of years lived by an 
                   individual in the specified age range within the scope of the calculation. 
                   For annual calculations (such as the ones in this application), this is the same 
                   as the current female population within the specified age range."),
              UI_GLOBALS$MMRATE_LATEX %>% withMathJax(),
              p("It is also commonly represented as the MMRate per 1,000 woman-years lived which is
                   the previous quantity multiplied by 1,000."),
              br(),
              h3("Data Sources"),
              data_source_tbl_html(),
              br(),
              p("Note: Pacific Islander/Non-Hispanic individuals are not available under the choice 
                  of ethnicity/race due to the fact that there were zero maternal deaths in this timeframe."),
              p("Note: The following race/ethnicity groups are included in the Other category as they 
                  consistently had very small counts of maternal deaths: American Indian Non-Hispanic, 
                  Other Single Race Non-Hispanic, and Other Two or More Races Non-Hispanic.")
            ),
            tabItem(
              tabName = "about",
              mainPanel(
                h3("This application was created through a partnership between the Women's Health Group
                 and Advanced Analytics."),
                br(),
                p(strong("Women's Health Group"), "is an enterprise level team with a vision to better the 
                health of women now and for future generations. Our mission is to deliver life-changing 
                healthcare solutions tailored for women through science, solutions, and collective action. 
                Our group is located within the Office of the Chief Medical Officer, which is an evidence 
                and science based, ethics and value driven, enterprise wide function at Johnson & Johnson."),
                p(strong("Advanced Analytics"), "is a cross functional team within Johnson & Johnson Technology 
                Services uniquely positioned to solve data science problems across the enterprise."),
                br()
              )
            )
          )
        )
      )
    )
  )
}
