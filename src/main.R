load("./massive_testing_strategies_functions.R")

h <- 5
w <- h * 1.6

df_strategy_1 <- covid_testing_strategies(strategy = 1)
plot_cost(df_strategy_1)
ggsave("graphs/plot_cost_strategy1.pdf", height = h, width = w)
plot_miss_positives(df_strategy_1)
ggsave("graphs/plot_positive_strategy1.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_1)
ggsave("graphs/plot_testsxperson_strategy1.pdf", height = h, width = w)

df_strategy_2_pool_5 <- covid_testing_strategies(strategy = 2, group_size = 5)
plot_cost(df_strategy_2_pool_5)
ggsave("graphs/plot_cost_strategy2_pool_5.pdf", height = h, width = w)
plot_miss_positives(df_strategy_2_pool_5)
ggsave("graphs/plot_positive_strategy2_pool_5.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_2_pool_5)
ggsave("graphs/plot_testsxperson_strategy2_pool_5.pdf", height = h, width = w)


# cbind(df_strategy_1,total_cost_pool_5=df_strategy_2_pool_5$total_cost) %>%
#   ggplot()+
#   geom_line(
#     aes(
#       x = prevalence,
#       y = total_cost_pool_5/total_cost-1,
#       color =  prob_symp_less_5d,
#       group =  prob_symp_less_5d
#     ),
#     size = 1
#   )  +
#      facet_grid(sens_model ~ spec_model) +
#   scale_y_continuous(sec.axis = sec_axis(
#     ~ . ,
#     name = "Classifier sensitivity",
#     breaks = NULL,
#     labels = NULL
#   )) +
#   scale_x_continuous(
#     labels = scales::number,
#     sec.axis = sec_axis(
#       ~ . ,
#       name = "Classifier specificity",
#       breaks = NULL,
#       labels = NULL
#     )
#   )




df_strategy_2_pool_10 <- covid_testing_strategies(strategy = 2, group_size = 10)
plot_cost(df_strategy_2_pool_10)
ggsave("graphs/plot_cost_strategy2_pool_10.pdf", height = h, width = w)
plot_miss_positives(df_strategy_2_pool_10)
ggsave("graphs/plot_positive_strategy2_pool_10.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_2_pool_10)
ggsave("graphs/plot_testsxperson_strategy2_pool_10.pdf", height = h, width = w)

df_strategy_3 <- covid_testing_strategies(strategy = 3)
plot_cost(df_strategy_3)
ggsave("graphs/plot_cost_strategy3.pdf", height = h, width = w)
plot_miss_positives(df_strategy_3)
ggsave("graphs/plot_positive_strategy3.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_3)
ggsave("graphs/plot_testsxperson_strategy3.pdf", height = h, width = w)


df_strategy_4 <- covid_testing_strategies(strategy = 4)
plot_cost(df_strategy_4)
ggsave("graphs/plot_cost_strategy4.pdf", height = h, width = w)
plot_miss_positives(df_strategy_4)
ggsave("graphs/plot_positive_strategy4.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_4)
ggsave("graphs/plot_testsxperson_strategy4.pdf", height = h, width = w)


df_strategy_5 <- covid_testing_strategies(strategy = 5, group_size = 5)
plot_cost(df_strategy_5)
ggsave("graphs/plot_cost_strategy5.pdf", height = h, width = w)
plot_miss_positives(df_strategy_5)
ggsave("graphs/plot_positive_strategy5.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_5)
ggsave("graphs/plot_testsxperson_strategy5.pdf", height = h, width = w)


df_strategy_6 <- covid_testing_strategies(strategy = 6)
plot_cost(df_strategy_6)
ggsave("graphs/plot_cost_strategy6.pdf", height = h, width = w)
plot_miss_positives(df_strategy_6)
ggsave("graphs/plot_positive_strategy6.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_6)
ggsave("graphs/plot_testsxperson_strategy6.pdf", height = h, width = w)



df_strategy_7 <- covid_testing_strategies(strategy = 7)
plot_cost(df_strategy_7)
ggsave("graphs/plot_cost_strategy7.pdf", height = h, width = w)
plot_miss_positives(df_strategy_7)
ggsave("graphs/plot_positive_strategy7.pdf", height = h, width = w)
plot_number_test_per_person(df_strategy_7)
ggsave("graphs/plot_testsxperson_strategy7.pdf", height = h, width = w)
