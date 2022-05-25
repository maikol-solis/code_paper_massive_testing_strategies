# Funcion de gráficos========================
#
#


plot_cost <- function(df_out) {
  if (df_out$strategy[1] <= 3) {
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
      )  +
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
          ~ . ,
          name = "Classifier sensitivity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      scale_x_continuous(
        labels = scales::number,
        sec.axis = sec_axis(
          ~ . ,
          name = "Classifier specificity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      labs(color = "Antigen tests",
           x = "Prevalence (%)",
           y = "Total cost ($)") +
      cowplot::theme_minimal_grid() +
      theme(
        panel.border = element_rect(linetype = "solid", color = "black"),
        legend.title.align = 1,
        legend.justification = "center"
      )
  } else {
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence,
      ) %>%
      ggplot() +
      geom_line(aes(x = prevalence,
                    y = total_cost),
                size = 2)  +
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
          ~ . ,
          name = "Classifier sensitivity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      scale_x_continuous(
        labels = scales::comma,
        sec.axis = sec_axis(
          ~ . ,
          name = "Classifier specificity",
          breaks = NULL,
          labels = NULL
        )
      ) +
      labs(x = "Prevalence (%)",
           y = "Total cost ($)") +
      cowplot::theme_minimal_grid() +
      theme(panel.border = element_rect(linetype = "solid", color = "black"))
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
      )  +
      geom_line(
        aes(x = prevalence, y = number_positive_theoretical),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(color = "Antigen tests",
           x = "Prevalence (%)",
           y = "Reported as positives") +
      cowplot::theme_minimal_grid() +
      theme(
        panel.border = element_rect(linetype = "solid", color = "black"),
        legend.title.align = 0.5
      )
  } else{
    df_out %>%
      mutate(
        sens_model = scales::percent(sens_model),
        spec_model = scales::percent(spec_model),
        prevalence = 100 * prevalence
      ) %>%
      ggplot() +
      geom_line(aes(x = prevalence, y = number_positive_reported),
                size = 2)  +
      geom_line(
        aes(x = prevalence, y = number_positive_theoretical),
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_y_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(x = "Prevalence (%)",
           y = "Expected positives") +
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
      )  +
      geom_hline(
        yintercept = 1,
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_color_continuous(label = scales::percent) +
      scale_y_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(color = "Antigen tests",
           x = "Prevalence (%)",
           y = "Tests per person") +
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
      geom_line(aes(x = prevalence,
                    y = number_test_per_person),
                size = 2)  +
      geom_hline(
        yintercept = 1,
        size = 1,
        linetype = "dashed",
        color = "red"
      ) +
      facet_grid(sens_model ~ spec_model) +
      scale_y_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier sensitivity",
        breaks = NULL,
        labels = NULL
      )) +
      scale_x_continuous(sec.axis = sec_axis(
        ~ . ,
        name = "Classifier specificity",
        breaks = NULL,
        labels = NULL
      )) +
      labs(x = "Prevalence (%)",
           y = "Tests per person") +
      cowplot::theme_minimal_grid() +
      theme(panel.border = element_rect(linetype = "solid", color = "black"))
  }
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
