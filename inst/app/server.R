shinyServer(function(input, output, session) {
  draws <- reactive({
    dat <- tibble(cond = c("A", "B"),
                  vis = c(input$a_vis, input$b_vis),
                  con = c(input$a_con, input$b_con))
    beta_dist <- calc_beta(x1 = input$lb, p1 = 0.025,
                           x2 = input$ub, p2 = 0.975)
    logit_prior <- logit_beta(beta_dist$shape1, beta_dist$shape2)
    mod_prior <- normal(location = logit_prior$location,
                        scale = logit_prior$scale)

    mod <- stan_glm(cbind(con, vis - con) ~ 0 + cond, data = dat,
                    family = binomial, iter = 4000, warmup = 2000,
                    chains = 4, refresh = 0,
                    prior = mod_prior)

    posterior_epred(mod, new_data = tibble(cond = c("A", "B"))) |>
      as_tibble(.name_repair = ~c("A", "B"))
  })

  output$posteriors <- renderPlot({
    req(draws())
    draws_dat <- draws()
    all_draws <- draws_dat |>
      pivot_longer(everything())

    x_limits <- c(min(all_draws$value) - 0.1,
                  max(all_draws$value) + 0.1)

    subtitle <- if (mean(draws_dat$A) > mean(draws_dat$B)) {
      glue("Condition A ({fmt_prop_pct(mean(draws_dat$A))}%) converted <b style='color:#FED766'>{fmt_prop_pct((mean(draws_dat$A) - mean(draws_dat$B)) / mean(draws_dat$B))}% better</b> than Condition B ({fmt_prop_pct(mean(draws_dat$B))}%).")
    } else {
      glue("Condition B ({fmt_prop_pct(mean(draws_dat$B))}%) converted <b style='color:#009FB7'>{fmt_prop_pct((mean(draws_dat$B) - mean(draws_dat$A)) / mean(draws_dat$A))}% better</b> than Condition A ({fmt_prop_pct(mean(draws_dat$A))}%).")
    }

    showtext_begin()
    all_draws |>
      ggplot(aes(x = value, y = fct_rev(name), fill = name)) +
      stat_halfeye(show.legend = FALSE) +
      scale_fill_manual(values = c("A" = palette_wjake[2],
                                   "B" = palette_wjake[1])) +
      expand_limits(x = x_limits) +
      scale_x_percent() +
      labs(x = "Estimated Conversion Rate",
           y = "Condition",
           subtitle = subtitle) +
      theme_wjake() +
      theme(axis.title.x = element_markdown(margin = margin(10, 0, 0, 0)),
            axis.title.y = element_markdown(margin = margin(0, 10, 0, 0)),
            plot.subtitle = element_markdown()) -> p
    print(p)
    showtext_end()
  }, res = 100)

  output$contrast <- renderPlot({
    req(draws())
    draws_dat <- draws()
    draws_contrast <- draws_dat |>
      mutate(contrast = A - B)

    a_best <- mean(draws_contrast$contrast > 0)

    subtitle <- if (a_best > 0.5) {
      glue("I'm {fmt_prop_pct(a_best)}% sure that <b style='color:#FED766'>Condition A</b> has a better conversation rate.")
    } else {
      glue("I'm {fmt_prop_pct(1 - a_best)}% sure that <b style='color:#009FB7'>Condition B</b> has a better conversation rate.")
    }

    showtext_begin()
    draws_contrast |>
      ggplot(aes(x = contrast, fill = after_stat(x > 0))) +
      stat_halfeye(show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = palette_wjake[2],
                                   "FALSE" = palette_wjake[1])) +
      labs(x = "Difference (A &minus; B)",
           y = "Density",
           subtitle = subtitle) +
      theme_wjake() +
      theme(axis.title.x = element_markdown(margin = margin(10, 0, 0, 0)),
            axis.title.y = element_markdown(margin = margin(0, 10, 0, 0)),
            plot.subtitle = element_markdown()) -> c
    print(c)
    showtext_end()
  }, res = 100)
})
