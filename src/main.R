library(tidyverse)
library(ggrepel)
source("src/massive_testing_strategies_functions.R")
source("src/plot_functions.R")

h <- 5
w <- h * 1.6

# Strategy 1 ----
df_strategy_1 <- covid_testing_strategies(strategy = 1)
plot_cost(df_strategy_1)
ggsave("graphs/plot_cost_strategy1.pdf",
  height = h,
  width = w
)
plot_miss_positives(df_strategy_1)
ggsave("graphs/plot_positive_strategy1.pdf",
  height = h,
  width = w
)
plot_number_test_per_person(df_strategy_1)
ggsave("graphs/plot_testsxperson_strategy1.pdf",
  height = h,
  width = w
)

# Strategy 2 ----
df_strategy_2_pool_5 <-
  covid_testing_strategies(strategy = 2, group_size = 5)
plot_cost(df_strategy_2_pool_5)
ggsave("graphs/plot_cost_strategy2_pool_5.pdf",
  height = h,
  width = w
)
plot_miss_positives(df_strategy_2_pool_5)
ggsave("graphs/plot_positive_strategy2_pool_5.pdf",
  height = h,
  width = w
)
plot_number_test_per_person(df_strategy_2_pool_5)
ggsave(
  "graphs/plot_testsxperson_strategy2_pool_5.pdf",
  height = h,
  width = w
)


# Strategy 3 ----
df_strategy_3 <- covid_testing_strategies(strategy = 3)
plot_cost(df_strategy_3)
ggsave("graphs/plot_cost_strategy3.pdf",
  height = h,
  width = w
)
plot_miss_positives(df_strategy_3)
ggsave("graphs/plot_positive_strategy3.pdf",
  height = h,
  width = w
)
plot_number_test_per_person(df_strategy_3)
ggsave("graphs/plot_testsxperson_strategy3.pdf",
  height = h,
  width = w
)

# Strategy 4 ----
df_strategy_4 <- covid_testing_strategies(strategy = 4)
plot_cost(df_strategy_4)
ggsave("graphs/plot_cost_strategy4.pdf",
  height = h,
  width = w
)
plot_miss_positives(df_strategy_4)
ggsave("graphs/plot_positive_strategy4.pdf",
  height = h,
  width = w
)
plot_number_test_per_person(df_strategy_4)
ggsave("graphs/plot_testsxperson_strategy4.pdf",
  height = h,
  width = w
)

# Combined graph -----
df_combined <- df_strategy_1 %>%
  full_join(df_strategy_2_pool_5) %>%
  full_join(df_strategy_3) %>%
  full_join(df_strategy_4)

plot_combined(df_combined)

ggsave(
  "graphs/plot_combined.pdf",
  dpi = 1200,
  height = h,
  width = w,
  scale = 1.2
)
