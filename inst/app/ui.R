shinyUI(
  dashboardPage(
    title = "A/B Testing",
    dashboardHeader(title = logo_grey_light, titleWidth = 200),
    dashboardSidebar(
      collapsed = TRUE,
      width = 200,
      sidebarMenu(
        menuItem("A/B Test", icon = icon("th"), tabName = "menu_top"),
        menuItem("Github", icon = icon("github"), href = "https://github.com/wjakethompson/abtest")
      )
    ),
    dashboardBody(
      theme_grey_light,
      tabItems(
        tabItem(
          tabName = "menu_top",
          fluidRow(
            box(title = HTML("<b>Inputs</b>"), width = 12, solidHeader = TRUE, status = "primary",
                fluidRow(column(2, p(HTML("<b>Lower Bound</b>"),
                                     span(shiny::icon("info-circle"),
                                          id = "info_lb"),
                                     numericInput('lb', NULL, .05),
                                     tippy::tippy_this(elementId = "info_lb",
                                                       tooltip = "Lower bound of expected conversion rates",
                                                       placement = "right"))),
                         column(2, p(HTML("<b>Upper Bound</b>"),
                                     span(shiny::icon("info-circle"),
                                          id = "info_ub"),
                                     numericInput('ub', NULL, .95),
                                     tippy::tippy_this(elementId = "info_ub",
                                                       tooltip = "Upper bound of expected conversion rates",
                                                       placement = "right"))),
                         column(1, p(HTML("<b>Condition A</b>"))),
                         column(3, p("Visitors", numericInput('a_vis', NULL, 0))),
                         column(3, p("Conversions", numericInput('a_con', NULL, 0)))),
                fluidRow(column(2), column(2),
                         column(1, p(HTML("<b>Condition B</b>"))),
                         column(3, p("Visitors", numericInput('b_vis', NULL, 0))),
                         column(3, p("Conversions", numericInput('b_con', NULL, 0))))
            ),
            box(
              title = HTML("<b>Test Result</b>",), width = 12, solidHeader = TRUE, status = "primary",
              column(6, plotOutput("posteriors")),
              column(6, plotOutput("contrast"))
            )
          )
        )
      )
    )
  )
)
