# Funcion de gráficos========================
#
#


plot_cost <- function(df_out) {
  if (df_out$strategy[1] %in% c(1, 2, 3)) {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence,
        # prob_symp_less_5d = 100 * prob_symp_less_5d
      ) %>%
      ggplot() +
      geom_line(
        aes(
          x = prevalence,
          y = total_cost,
          color =  prob_symp_less_5d,
          group =  prob_symp_less_5d
        ),
        size = 1
      ) +
      geom_line(
        aes(x = prevalence, y = 1000 * prevalence),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      # geom_hline(
      #   yintercept = 100 * 1000,
      #   size = 1,
      #   linetype = "dashed",
      #   color = "red"
      # ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(
        labels = scales::comma,
        sec.axis = sec_axis(
          ~.,
          name = "Classifier sensitivity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      scale_x_continuous(
        labels = scales::number,
        sec.axis = sec_axis(
          ~.,
          name = "Classifier specificity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      labs(
        color = "Antigen tests",
        x = "Prevalence (%)",
        y = "Total cost ($)"
      ) +
      cowplot::theme_minimal_grid() +
      theme(
        panel.border = element_rect(linetype = "solid", color = "black"),
        legend.title.align = 1,
        legend.justification = "center"
      )
  } else if (df_out$strategy[1] %in% c(4, 5, 6)) {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence,
      ) %>%
      ggplot() +
      geom_line(aes(
        x = prevalence,
        y = total_cost
      ),
      size = 2
      ) +
      geom_line(
        aes(x = prevalence, y = 1000 * prevalence),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(
        labels = scales::comma,
        sec.axis = sec_axis(
          ~.,
          name = "Classifier sensitivity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      scale_x_continuous(
        labels = scales::comma,
        sec.axis = sec_axis(
          ~.,
          name = "Classifier specificity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      labs(
        x = "Prevalence (%)",
        y = "Total cost ($)"
      ) +
      cowplot::theme_minimal_grid() +
      theme(panel.border = element_rect(linetype = "solid", color = "black"))
  } else if (df_out$strategy[1] == 4) {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence,
        # prob_symp_less_5d = 100 * prob_symp_less_5d
      ) %>%
      ggplot() +
      geom_line(
        aes(
          x = prevalence,
          y = total_cost,
          color =  prob_model_first_responder_pcr,
          group =  prob_model_first_responder_pcr
        ),
        size = 1
      ) +
      geom_line(
        aes(x = prevalence, y = 1000 * prevalence),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(
        labels = scales::comma,
        sec.axis = sec_axis(
          ~.,
          name = "Classifier sensitivity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      scale_x_continuous(
        labels = scales::number,
        sec.axis = sec_axis(
          ~.,
          name = "Classifier specificity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      labs(
        color = "Essential Workers",
        x = "Prevalence (%)",
        y = "Total cost ($)"
      ) +
      cowplot::theme_minimal_grid() +
      theme(
        panel.border = element_rect(linetype = "solid", color = "black"),
        legend.title.align = 1,
        legend.justification = "center"
      )
  }
}

plot_miss_positives <- function(df_out) {
  if (df_out$strategy[1] <= 3) {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence
      ) %>%
      ggplot() +
      geom_line(
        aes(
          x = prevalence,
          y = number_positive_reported,
          # - number_positive_theoretical,
          color = prob_symp_less_5d,
          group = prob_symp_less_5d
        ),
        size = 1
      ) +
      geom_line(
        aes(x = prevalence, y = number_positive_theoretical),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(
        color = "Antigen tests",
        x = "Prevalence (%)",
        y = "Reported as positives"
      ) +
      cowplot::theme_minimal_grid() +
      theme(
        panel.border = element_rect(linetype = "solid", color = "black"),
        legend.title.align = 0.5
      )
  } else {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence
      ) %>%
      ggplot() +
      geom_line(aes(x = prevalence, y = number_positive_reported),
        size = 2
      ) +
      geom_line(
        aes(x = prevalence, y = number_positive_theoretical),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_y_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(
        x = "Prevalence (%)",
        y = "Expected positives"
      ) +
      cowplot::theme_minimal_grid() +
      theme(panel.border = element_rect(linetype = "solid", color = "black"))
  }
}


plot_number_test_per_person <- function(df_out) {
  if (df_out$strategy[1] <= 3) {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence
      ) %>%
      ggplot() +
      geom_line(
        aes(
          x = prevalence,
          y = number_test_per_person,
          color = prob_symp_less_5d,
          group = prob_symp_less_5d
        ),
        size = 1
      ) +
      geom_hline(
        yintercept = 1,
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(
        color = "Antigen tests",
        x = "Prevalence (%)",
        y = "Tests per person"
      ) +
      cowplot::theme_minimal_grid() +
      theme(
        panel.border = element_rect(linetype = "solid", color = "black"),
        legend.title.align = 0.5
      )
  } else {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence
      ) %>%
      ggplot() +
      geom_line(aes(
        x = prevalence,
        y = number_test_per_person
      ),
      size = 2
      ) +
      geom_hline(
        yintercept = 1,
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_y_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~.,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(
        x = "Prevalence (%)",
        y = "Tests per person"
      ) +
      cowplot::theme_minimal_grid() +
      theme(panel.border = element_rect(linetype = "solid", color = "black"))
  }
}

plot_combined <- function(df_combined) {

  # data.frame plot combined 1 ----
  df_plot_combined <- df_combined %>%
    replace(is.na(.), 0) %>%
    filter(sens_model %in% c(0.6, 0.9), spec_model %in% c(0.6, 0.9)) %>%
    mutate(
      sens_model = scales::percent(sens_model),
      spec_model = scales::percent(spec_model),
      # prevalence = 100 * prevalence,
      efficiency = number_positive_reported / total_cost * ((people_high_risk +
        people_low_risk) / 1000),
      stock = (number_alt_tests + number_pcr_tests) /
        total_cost * ((people_high_risk +
          people_low_risk) / 1000),
      true_efficiency = (1000 * prevalence) / (100 * 1000 * prevalence) *
        ((people_high_risk + people_low_risk) / 1000),
      true_amortization = (1000 * prevalence) / (100 * 1000 * prevalence) *
        ((people_high_risk + people_low_risk) / 1000)
    )

  # plot combined 1 ----
  ggplot(
    data = df_plot_combined,
    mapping = aes(
      y = stock,
      x = efficiency,
      color =  prob_symp_less_5d,
      group =  prob_symp_less_5d
    )
  ) +
    geom_path(
      size = 1,
      arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
      linejoin = "round"
    ) +
    geom_path(aes(
      x = true_efficiency,
      y = true_amortization
    ),
    color = "red",
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    facet_grid(strategy ~ sens_model + spec_model,
      labeller = labeller(
        sens_model = ~ paste("Sens: ", .),
        spec_model = ~ paste("Spec: ", .),
        .multi_line = TRUE
      )
    ) +
    scale_color_continuous(label = scales::percent) +
    scale_y_continuous(
      labels = scales::comma_format(scale = 100000),
      sec.axis = sec_axis(
        ~.,
        name = "Strategy",
        breaks = NULL,
        labels = NULL
      )
    ) +
    scale_x_continuous(
      labels = scales::comma_format(scale = 100000),
      # breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01),
      sec.axis = sec_axis(
        ~.,
        name = "Classifier performance",
        breaks = NULL,
        labels = NULL
      )
    ) +
    # coord_fixed(ratio = 0.15) +
    labs(
      color = "Antigen test",
      caption = "Values assuming a budget of $100,000.",
      y = "Stock capacity",
      x = "Detection efficiency"
    ) +
    cowplot::theme_minimal_grid() +
    theme(
      panel.border = element_rect(linetype = "solid", color = "black"),
      legend.title.align = 1,
      legend.justification = "center",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#   plt <-
#     ggplot(df_out.clean,
#            aes(x = prevalence, y = cost_total, color = Classifier sensitivity_vec)) +
#     geom_line(size = 1.5) +
#     facet_grid(. ~ Classifier specificity_vec) +
#     # facet_grid(Classifier specificity~Classifier sensitivity ) +
#     scale_y_continuous(labels = scales::dollar) +
#     scale_x_continuous(labels = scales::number_format(scale = 100, accuracy = 1)) +
#     labs(
#       color = "Classifier sensitivity",
#       subtitle = "Classifier specificity",
#       x = "Prevalence (%)",
#       y = "Total cost"
#     ) +
#     scale_color_stata() +
#     theme_cowplot(font_size = 30) +
#     background_grid() +
#     panel_border() +
#     theme(plot.subtitle = element_text(hjust = 0.5))
#   # theme_minimal_grid(font_size = 18) +
#   # theme(
#   #   panel.grid.minor.y = element_line(color = "grey90"),
#   #   panel.grid.major.y = element_line(color = "grey70")
#   # )
#   plt
# }
