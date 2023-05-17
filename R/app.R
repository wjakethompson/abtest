library(shiny)

#' @importFrom rlang .data
#' @export
abTest <- function(...) {
  ui <- shinydashboard::dashboardPage(
    title = "A/B Testing",
    shinydashboard::dashboardHeader(title = logo_grey_light, titleWidth = 200),
    shinydashboard::dashboardSidebar(
      collapsed = TRUE,
      width = 200,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("A/B Test", icon = shiny::icon("th"), tabName = "menu_top"),
        shinydashboard::menuItem("Github", icon = shiny::icon("github"), href = "https://github.com/wjakethompson/abtest")
      )
    ),
    shinydashboard::dashboardBody(
      theme_wjake_shiny,
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "menu_top",
          shiny::fluidRow(
            shinydashboard::box(title = htmltools::HTML("<b>Inputs</b>"), width = 12, solidHeader = TRUE, status = "primary",
                                shiny::fluidRow(shiny::column(2, shiny::p(htmltools::HTML("<b>Lower Bound</b>"),
                                                                          htmltools::span(shiny::icon("info-circle"),
                                                                                          id = "info_lb"),
                                                                          shiny::numericInput('lb', NULL, .05),
                                                                          tippy::tippy_this(elementId = "info_lb",
                                                                                            tooltip = "Lower bound of expected conversion rates",
                                                                                            placement = "right"))),
                                                shiny::column(2, shiny::p(htmltools::HTML("<b>Upper Bound</b>"),
                                                                          htmltools::span(shiny::icon("info-circle"),
                                                                                          id = "info_ub"),
                                                                          shiny::numericInput('ub', NULL, .95),
                                                                          tippy::tippy_this(elementId = "info_ub",
                                                                                            tooltip = "Upper bound of expected conversion rates",
                                                                                            placement = "right"))),
                                                shiny::column(1, shiny::p(htmltools::HTML("<b>Condition A</b>"))),
                                                shiny::column(3, shiny::p("Visitors", shiny::numericInput('a_vis', NULL, 0))),
                                                shiny::column(3, shiny::p("Conversions", shiny::numericInput('a_con', NULL, 0)))),
                                shiny::fluidRow(shiny::column(2), shiny::column(2),
                                                shiny::column(1, shiny::p(htmltools::HTML("<b>Condition B</b>"))),
                                                shiny::column(3, shiny::p("Visitors", shiny::numericInput('b_vis', NULL, 0))),
                                                shiny::column(3, shiny::p("Conversions", shiny::numericInput('b_con', NULL, 0))))
            ),
            shinydashboard::box(
              title = htmltools::HTML("<b>Test Result</b>",), width = 12, solidHeader = TRUE, status = "primary",
              shiny::column(6, shiny::plotOutput("posteriors")),
              shiny::column(6, shiny::plotOutput("contrast"))
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    draws <- shiny::reactive({
      dat <- tibble::tibble(cond = c("A", "B"),
                            vis = c(input$a_vis, input$b_vis),
                            con = c(input$a_con, input$b_con))
      beta_dist <- calc_beta(x1 = input$lb, p1 = 0.025,
                             x2 = input$ub, p2 = 0.975)
      logit_prior <- logit_beta(beta_dist$shape1, beta_dist$shape2)
      mod_prior <- rstanarm::normal(location = logit_prior$location,
                                    scale = logit_prior$scale)

      mod <- rstanarm::stan_glm(cbind(con, vis - con) ~ 0 + cond, data = dat,
                                family = "binomial", iter = 4000, warmup = 2000,
                                chains = 4, refresh = 0,
                                prior = mod_prior)

      rstanarm::posterior_epred(mod, new_data = tibble::tibble(cond = c("A", "B"))) |>
        tibble::as_tibble(.name_repair = ~c("A", "B"))
    })

    output$posteriors <- shiny::renderPlot({
      shiny::req(draws())
      draws_dat <- draws()
      all_draws <- draws_dat |>
        tidyr::pivot_longer(dplyr::everything())

      x_limits <- c(min(all_draws$value) - 0.1,
                    max(all_draws$value) + 0.1)

      subtitle <- if (mean(draws_dat$A) > mean(draws_dat$B)) {
        glue::glue("Condition A ({wjake::fmt_prop_pct(mean(draws_dat$A))}%) converted <b style='color:#FED766'>{wjake::fmt_prop_pct((mean(draws_dat$A) - mean(draws_dat$B)) / mean(draws_dat$B))}% better</b> than Condition B ({wjake::fmt_prop_pct(mean(draws_dat$B))}%).")
      } else {
        glue::glue("Condition B ({wjake::fmt_prop_pct(mean(draws_dat$B))}%) converted <b style='color:#009FB7'>{wjake::fmt_prop_pct((mean(draws_dat$B) - mean(draws_dat$A)) / mean(draws_dat$A))}% better</b> than Condition A ({wjake::fmt_prop_pct(mean(draws_dat$A))}%).")
      }

      showtext::showtext_begin()
      all_draws |>
        ggplot2::ggplot(ggplot2::aes(x = .data$value, y = forcats::fct_rev(.data$name), fill = .data$name)) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c("A" = wjake::palette_wjake[2],
                                              "B" = wjake::palette_wjake[1])) +
        ggplot2::expand_limits(x = x_limits) +
        wjake::scale_x_percent() +
        ggplot2::labs(x = "Estimated Conversion Rate",
                      y = "Condition",
                      subtitle = subtitle) +
        wjake::theme_wjake() +
        ggplot2::theme(axis.title.x = ggtext::element_markdown(margin = ggplot2::margin(10, 0, 0, 0)),
                       axis.title.y = ggtext::element_markdown(margin = ggplot2::margin(0, 10, 0, 0)),
                       plot.subtitle = ggtext::element_markdown()) -> p
      print(p)
      showtext::showtext_end()
    }, res = 100)

    output$contrast <- shiny::renderPlot({
      shiny::req(draws())
      draws_dat <- draws()
      draws_contrast <- draws_dat |>
        dplyr::mutate(contrast = .data$A - .data$B)

      a_best <- mean(draws_contrast$contrast > 0)

      subtitle <- if (a_best > 0.5) {
        glue::glue("I'm {wjake::fmt_prop_pct(a_best)}% sure that <b style='color:#FED766'>Condition A</b> has a better conversation rate.")
      } else {
        glue::glue("I'm {wjake::fmt_prop_pct(1 - a_best)}% sure that <b style='color:#009FB7'>Condition B</b> has a better conversation rate.")
      }

      showtext::showtext_begin()
      draws_contrast |>
        ggplot2::ggplot(ggplot2::aes(x = .data$contrast, fill = ggplot2::after_stat(x > 0))) +
        ggdist::stat_halfeye(show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c("TRUE" = wjake::palette_wjake[2],
                                     "FALSE" = wjake::palette_wjake[1])) +
        ggplot2::labs(x = "Difference (A &minus; B)",
             y = "Density",
             subtitle = subtitle) +
        wjake::theme_wjake() +
        ggplot2::theme(axis.title.x = ggtext::element_markdown(margin = ggplot2::margin(10, 0, 0, 0)),
                       axis.title.y = ggtext::element_markdown(margin = ggplot2::margin(0, 10, 0, 0)),
                       plot.subtitle = ggtext::element_markdown()) -> c
      print(c)
      showtext::showtext_end()
    }, res = 100)
  }
  sysfonts::font_add_google("Source Sans Pro", "Source Sans Pro")
  shiny::shinyApp(ui, server, ...)
}

