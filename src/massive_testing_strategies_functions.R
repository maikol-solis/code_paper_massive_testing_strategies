# Pooling====

# GROUP TESTING
# CREATED: Eli P. Fenichel, Yale University
# LAST UPDATED: July 15, 2020
# Probability of a groups testing positive accounting for sensitivity of test
# with prevalence m, group size = group, population = pop
estimate_pooling_prob_of_pos_groups <-
  function(group_size, m, pop, se, s_loss) {
    shat <- se - group_size * s_loss
    p_pos <-
      1 - (1 - shat * m)^group_size # shat es la sensibilidad,
    # p.pos: Probabilidad que un grupo sea positivo
    # (1-prevalencia): Probabilidad que se salga negativo una persona
    # (1-prevalencia)^tamano_grupo: Probabilidad que todos sean negativos en el grupo
    return(p_pos)
  }


# Group results
estimate_pooling_tests <-
  function(group_size, p, pop, se, s_loss, sp) {
    p_pos_pool <-
      estimate_pooling_prob_of_pos_groups(group_size, p, pop, se, s_loss)
    p_pos_test <- se * p + (1 - sp) * (1 - p)
    # number of groups
    n_group <- pop / group_size
    pos_groups <- p_pos_pool * n_group
    neg_groups <- n_group - pos_groups

    # number of people
    neg_peop_group <- neg_groups * group_size
    pos_peop_group <- pos_groups * group_size
    pos_peop <- pos_peop_group * p_pos_test
    n_groups_pooling <- pos_groups + neg_groups
    total_tests_pooling <- n_groups_pooling + pos_peop
    test_per_person_pooling <- total_tests_pooling / pop

    # the expected number of people in negative groups that are actually
    # false positives with group size = group.size, prevalence p, sensitivity s,
    # in a population p.
    shat <- se - group_size * s_loss
    t_prob <- (1 - shat) * p + (1 - p)
    expected_number_missed <- sapply(
      c(1:(group_size - 1)),
      function(x) {
        choose(group_size, group_size - x) *
          ((1 - p) / t_prob)^(group_size - x) *
          ((1 - shat) * p / t_prob)^x
      }
    )

    exp_m <- t(c(1:(group_size - 1))) %*% expected_number_missed
    gm <- exp_m * neg_groups



    res <- data.frame(
      "p_pos_pool" = p_pos_pool,
      "p_pos_test" = p_pos_test,
      "pos_groups" = pos_groups,
      "neg_groups" = neg_groups,
      "pos_peop_group" = pos_peop_group,
      "pos_peop" = pos_peop,
      "neg_peop_group" = neg_peop_group,
      "n_groups_pooling" = n_groups_pooling,
      "n_undetected_pos" = gm,
      "undetected_groups" = exp_m,
      "total_tests_pooling" = total_tests_pooling,
      "number_test_per_person_pooling" = test_per_person_pooling
    )

    return(res)
  }





covid_testing_strategies <-
  function(prevalence = seq(0, 0.3, length.out = 500),
           sens_model = c(0.3, 0.6, 0.9),
           spec_model = c(0.3, 0.6, 0.9),
           sens_ag_test = 0.80,
           spec_ag_test = 0.95,
           sens_pcr_test = 0.95,
           spec_pcr_test = 0.95,
           sens_lamp_test = 0.95,
           spec_lamp_test = 0.95,
           people = 1000,
           cost_pcr = 100,
           cost_antigen = 50,
           cost_lamp = 20,
           group_size = 5,
           strategy = 1) {
    df_out <- data.frame(prevalence = numeric())

    # df_out <- data.frame(
    #   prevalence = numeric(),
    #   sens_model = numeric(),
    #   spec_model = numeric(),
    #   sens_ag_test = numeric(),
    #   spec_ag_test = numeric(),
    #   prob_symp_less_5d = numeric(),
    #   prob_symp_more_5d = numeric(),
    #   number_alt_tests = numeric(),
    #   number_pcr_tests = numeric(),
    #   number_positive_reported = numeric(),
    #   number_positive_theoretical = numeric(),
    #   number_test_per_person = numeric() ,
    #   total_cost_alt = numeric(),
    #   total_cost_pcr = numeric(),
    #   total_cost = numeric()
    # )

    for (sens in sens_model) {
      for (spec in spec_model) {
        # Setup --------

        # Probability that an a test gives a positive result
        ## Antigen
        prob_res_pos_ag <- sens_ag_test * prevalence +
          (1 - spec_ag_test) * (1 - prevalence)
        ## LAMP
        prob_res_pos_lamp <- sens_lamp_test * prevalence +
          (1 - spec_lamp_test) * (1 - prevalence)
        ## PCR
        prob_res_pos_pcr <- sens_pcr_test * prevalence +
          (1 - spec_pcr_test) * (1 - prevalence)


        # Probability that a test gives a negative result
        prob_res_neg_ag <- 1 - prob_res_pos_ag
        prob_res_neg_lamp <- 1 - prob_res_pos_lamp
        prob_res_neg_pcr <- 1 - prob_res_pos_pcr


        ## Probability model gives a low risk classification
        # prob_model_low_risk <- (1 - sens) * prevalence +
        #   spec * (1 - prevalence)
        # prob_model_high_risk <- 1 - prob_model_low_risk

        prob_model_high_risk_ag <- sens * prob_res_pos_ag +
          (1 - spec) * prob_res_neg_ag
        # prob_model_high_risk_ag <- 1 - prob_model_low_risk_ag

        prob_model_high_risk_pcr <- sens * prob_res_pos_pcr +
          (1 - spec) * prob_res_neg_pcr
        # prob_model_high_risk_pcr <- 1 - prob_model_low_risk_pcr

        q_model_high_risk_ag <- qlogis(prob_model_high_risk_ag)
        q_model_high_risk_pcr <- qlogis(prob_model_high_risk_pcr)

        prob_model_high_risk <- plogis(0.5 * (q_model_high_risk_ag
        + q_model_high_risk_pcr))
        prob_model_low_risk <- 1 - prob_model_high_risk



        # PPV y NPV of model
        npv_model_ag <- spec * prob_res_neg_ag / prob_model_low_risk
        npv_model_pcr <-
          spec * prob_res_neg_pcr / prob_model_low_risk

        q_npv_model_low_risk_ag <- qlogis(npv_model_ag)
        q_npv_model_low_risk_pcr <- qlogis(npv_model_pcr)

        npv_model <- plogis(0.5 * (q_npv_model_low_risk_ag
        + q_npv_model_low_risk_pcr))
        #
        # npv_model <- spec * (1 - prevalence) /
        #   ((1 - sens) * prevalence + spec * (1 - prevalence))


        # People in risk
        people_high_risk <- people * prob_model_high_risk
        people_low_risk <- people * prob_model_low_risk

        # Probability that given High risk classification
        # the patient has a positive result with antigen
        # prob_res_pos_given_high_risk <- sens * prob_res_pos_ag /
        #   prob_model_high_risk

        prob_res_pos_given_high_risk_ag <- sens * prob_res_pos_ag /
          prob_model_high_risk

        prob_res_neg_given_high_risk_ag <-
          (1 - spec) * prob_res_neg_ag /
            prob_model_high_risk

        prob_res_pos_given_low_risk_ag <-
          (1 - sens) * prob_res_pos_ag /
            prob_model_low_risk

        ## prob_res_pos_given_high_risk_lamp <- sens * prob_res_pos_lamp /
        ## prob_model_high_risk

        prob_res_pos_given_high_risk_lamp <-
          sens * prob_res_pos_lamp /
            prob_model_high_risk

        prob_res_neg_given_high_risk_lamp <-
          1 - prob_res_pos_given_high_risk_lamp

        prob_res_pos_given_high_risk_pcr <-
          sens * prob_res_pos_pcr /
            prob_model_high_risk

        prob_res_neg_given_high_risk_pcr <-
          1 - prob_res_pos_given_high_risk_pcr

        # prob_res_pos_given_high_risk_lamp <-
        #   sens * prob_res_pos_lamp / prob_model_high_risk
        #
        # prob_res_neg_given_high_risk_lamp <-
        #   (1 - spec) * prob_res_neg_lamp / prob_model_high_risk
        #
        # prob_res_neg_given_low_risk_lamp <-
        #   spec * prob_res_neg_lamp / prob_model_low_risk
        #
        # prob_res_pos_given_low_risk_lamp <-
        #   (1 - sens) * prob_res_pos_lamp / prob_model_high_risk

        # Probability that given High risk classification
        # the patient has a negative result with antigen
        prob_res_neg_given_high_risk_ag <-
          (1 - spec) * prob_res_neg_ag /
            prob_model_high_risk

        # PPV golden test
        ppv_golden_test <- sens_pcr_test * prevalence /
          (sens_pcr_test * prevalence
            + (1 - spec_pcr_test * (1 - prevalence)))



        # Strategy 1 -------------------------------------------------------
        if (strategy == 1) {
          ## Probability of people tested after of 5 days since symptoms onset
          for (prob_symp_more_5d in c(0.25, 0.5, 0.75)) {
            ## Probability of people tested before of 5 days since symptoms onset.
            prob_symp_less_5d <- 1 - prob_symp_more_5d

            people_low_risk <- 0

            # Number of antigen tests done
            number_alt_tests <- people_high_risk * prob_symp_less_5d


            # Number of PCR tests done
            number_pcr_tests <-
              people_high_risk * (prob_symp_more_5d +
                prob_symp_less_5d * prob_res_neg_given_high_risk_ag)

            ## number_positive_reported <- people_high_risk * (
            ##   prob_symp_less_5d * (
            ##     prob_res_pos_given_high_risk +
            ##       prob_res_neg_given_high_risk * prob_res_pos_pcr
            ##   ) +
            ##     prob_symp_more_5d * prob_res_pos_pcr
            ## )

            # number_positive_reported <- number_pcr_tests * prob_res_pos_pcr +
            #   number_alt_tests * prob_res_pos_ag

            number_positive_reported <-
              people_high_risk * prob_symp_less_5d * (
                prob_res_pos_given_high_risk_ag +
                  prob_res_neg_given_high_risk_ag * prob_res_pos_given_high_risk_pcr
              ) +
              people_high_risk * prob_symp_more_5d * prob_res_pos_given_high_risk_pcr

            # if (sens == 0.9 & spec == 0.9) {
            #   browser()
            # }
            number_test_per_person <-
              (number_pcr_tests + number_alt_tests) /
                (people * prob_model_high_risk)

            # Total costs.
            total_cost_alt <- cost_antigen * number_alt_tests
            total_cost_pcr <- cost_pcr * number_pcr_tests
            total_cost <- total_cost_alt + total_cost_pcr

            df_out <- rbind(
              df_out,
              data.frame(
                prevalence,
                people_high_risk,
                people_low_risk,
                sens_model = sens,
                spec_model = spec,
                sens_ag_test,
                spec_ag_test,
                prob_symp_less_5d,
                prob_symp_more_5d,
                number_alt_tests,
                number_pcr_tests,
                number_positive_reported,
                number_positive_theoretical = people * prevalence,
                number_test_per_person,
                total_cost_alt,
                total_cost_pcr,
                total_cost
              )
            )
          }
        }

        # Strategy 2 --------------------------------------------------------

        else if (strategy == 2) {
          ## Probability of people tested after of 5 days since symptoms onset
          for (prob_symp_more_5d in c(0.25, 0.5, 0.75)) {
            ## Probability of people tested before of 5 days since symptoms onset.
            prob_symp_less_5d <- 1 - prob_symp_more_5d

            pooling_tests_full_prevalence <- mapply(
              FUN = function(prev, pop) {
                estimate_pooling_tests(
                  group_size = group_size,
                  p = prev,
                  pop = pop,
                  # s = sens,
                  se = sens_pcr_test,
                  s_loss = 0,
                  sp = spec_pcr_test
                )
              },
              prev = prevalence,
              pop = people_low_risk,
              SIMPLIFY = FALSE
            )

            pooling_tests_full_prevalence <- do.call(
              "rbind",
              pooling_tests_full_prevalence
            )



            pooling_tests_low_risk <- mapply(
              FUN = function(prev, pop) {
                estimate_pooling_tests(
                  group_size = group_size,
                  p = prev,
                  pop = pop,
                  # s = sens,
                  se = sens_pcr_test,
                  s_loss = 0,
                  sp = spec_pcr_test
                )
              },
              prev = 1 - npv_model,
              pop = people_low_risk,
              SIMPLIFY = FALSE
            )

            pooling_tests_low_risk <-
              do.call("rbind", pooling_tests_low_risk)


            # Number of antigen tests done
            number_alt_tests <- people_high_risk * prob_symp_less_5d


            # Number of PCR tests done
            number_pcr_tests <- people_high_risk *
              (prob_symp_more_5d +
                prob_symp_less_5d *
                  prob_res_neg_given_high_risk_ag) +
              pooling_tests_low_risk$total_tests_pooling


            ## number_positive_reported <-
            ##   people_high_risk * (
            ##     prob_symp_less_5d * (
            ##       prob_res_pos_given_high_risk +
            ##         prob_res_neg_given_high_risk * prob_res_pos_pcr
            ##     ) +
            ##       prob_symp_more_5d * prob_res_pos_pcr
            ##   ) + pooling_tests_low_risk$pos_peop

            # if (sens == 0.9 & spec == 0.9) {
            #   browser()
            # }
            number_positive_reported <-
              people_high_risk * prob_symp_less_5d *
              (
                prob_res_pos_given_high_risk_ag +
                  prob_res_neg_given_high_risk_ag *
                    prob_res_pos_given_high_risk_pcr
              ) +
              people_high_risk * prob_symp_more_5d *
                prob_res_pos_given_high_risk_pcr +
              pooling_tests_low_risk$pos_peop






            # ifelse(
            #   pooling_tests[, "total_tests_pooling"] > prob_model_low_risk,
            #   ppv_model,
            #   pooling_tests[, "pos_peop"]
            # )

            number_test_per_person <-
              (number_pcr_tests + number_alt_tests) /
                people

            # Total costs.
            total_cost_alt <- cost_antigen * number_alt_tests
            total_cost_pcr <- cost_pcr * number_pcr_tests
            total_cost <- total_cost_alt + total_cost_pcr

            df_out <- rbind(
              df_out,
              cbind(
                data.frame(
                  prevalence,
                  people_high_risk,
                  people_low_risk,
                  sens_model = sens,
                  spec_model = spec,
                  sens_ag_test,
                  spec_ag_test,
                  prob_symp_less_5d,
                  prob_symp_more_5d,
                  number_alt_tests,
                  number_pcr_tests,
                  number_positive_reported,
                  number_positive_theoretical = people * prevalence,
                  number_test_per_person,
                  total_cost_alt,
                  total_cost_pcr,
                  total_cost,
                  total_test_full_prevalence = pooling_tests_full_prevalence$total_tests_pooling
                ),
                pooling_tests_low_risk
              )
            )
          }
        }
        # Strategy 3 ---------------------------------------------------------
        else if (strategy == 3) {
          ## Probability of people tested after of 5 days since symptoms onset
          for (prob_symp_more_5d in c(0.25, 0.5, 0.75)) {
            ## Probability of people tested before of 5 days since symptoms onset.
            prob_symp_less_5d <- 1 - prob_symp_more_5d

            # Number of antigen tests done
            number_alt_tests <-
              people_high_risk * prob_symp_less_5d +
              people_low_risk * (1 + prob_res_pos_given_low_risk_ag)


            # Number of PCR tests done
            number_pcr_tests <- people_high_risk *
              (prob_symp_more_5d +
                prob_symp_less_5d * prob_res_neg_given_high_risk_ag)

            number_positive_reported <-
              people_high_risk * prob_symp_less_5d *
              (
                prob_res_pos_given_high_risk_ag +
                  prob_res_neg_given_high_risk_ag *
                    prob_res_pos_given_high_risk_pcr
              ) +
              people_high_risk * prob_symp_more_5d *
                prob_res_pos_given_high_risk_pcr +
              people_low_risk *
                prob_res_pos_given_low_risk_ag * prob_res_pos_given_low_risk_ag





            number_pcr_tests * prob_res_pos_pcr +
              number_alt_tests * prob_res_pos_ag

            ## number_positive_reported <- people_high_risk *
            ##   (prob_symp_less_5d *
            ##     (prob_res_pos_given_high_risk +
            ##       prob_res_neg_given_high_risk * prob_res_pos_pcr) +
            ##     prob_symp_more_5d * prob_res_pos_pcr) +
            ##   people_low_risk *
            ##     (prob_res_pos_given_low_risk +
            ##      prob_res_neg_given_low_risk * prob_res_pos_ag)

            number_test_per_person <-
              (number_pcr_tests + number_alt_tests) /
                (people)

            # Total costs.
            total_cost_alt <- cost_antigen * number_alt_tests
            total_cost_pcr <- cost_pcr * number_pcr_tests
            total_cost <- total_cost_alt + total_cost_pcr

            df_out <- rbind(
              df_out,
              data.frame(
                prevalence,
                people_high_risk,
                people_low_risk,
                sens_model = sens,
                spec_model = spec,
                sens_ag_test,
                spec_ag_test,
                prob_symp_less_5d,
                prob_symp_more_5d,
                number_alt_tests,
                number_pcr_tests,
                number_positive_reported,
                number_positive_theoretical = people * prevalence,
                number_test_per_person,
                total_cost_alt,
                total_cost_pcr,
                total_cost
              )
            )
          }
        }
        # Strategy 4 -----------------------------------------------------------
        else if (strategy == 4) {
          prob_model_first_responder_pcr <- 0.01
          ## Probability of model
          ## prob_model_first_responder_pcr <- sens * prob_res_pos_pcr +
          ##   (1 - spec) * prob_res_neg_pcr


          # prob_model_high_risk_ag <- sens * prob_res_pos_ag +
          #   (1 - spec) * prob_res_neg_ag
          #
          # prob_model_low_risk_ag <- 1 - prob_model_high_risk_ag
          #
          # prob_model_high_risk_lamp <- sens * prob_res_pos_lamp +
          #   (1 - spec) * prob_res_neg_lamp
          #
          # q_model_high_risk_ag <- qlogis(prob_model_high_risk_ag)
          # q_model_high_risk_lamp <- qlogis(prob_model_high_risk_lamp)
          #
          # prob_model_high_risk <- plogis(0.5 * (q_model_high_risk_ag
          # + q_model_high_risk_lamp))
          # prob_model_low_risk <- 1 - prob_model_high_risk


          # prob_res_pos_given_low_risk_ag <-
          #   (1 - sens) * prob_res_pos_ag / prob_model_low_risk_ag
          #
          # prob_res_neg_given_high_risk_lamp <-
          #   (1 - spec) * prob_res_neg_lamp / prob_model_high_risk_lamp
          #
          # prob_res_pos_given_high_risk_lamp <-
          #   1 - prob_res_neg_given_high_risk_lamp
          ## People

          people_first_responder <-
            people * prob_model_first_responder_pcr
          people_high_risk <- (people - people_first_responder) *
            prob_model_high_risk
          people_low_risk <- (people - people_first_responder) *
            prob_model_low_risk

          ## Number of tests
          number_pcr_tests <- people_first_responder

          number_lamp_tests <- people_high_risk *
            (1 + prob_res_neg_given_high_risk_lamp)

          number_ag_tests <- people_low_risk *
            (1 + prob_res_pos_given_low_risk_ag)

          number_alt_tests <- number_lamp_tests + number_ag_tests

          number_positive_reported <-
            # First responder
            people_first_responder * prob_res_pos_pcr +
            # High risk
            people_high_risk * prob_res_pos_given_high_risk_pcr *
              # (1 + prob_res_neg_given_high_risk_pcr) +
              (1 + (1 - prob_res_pos_given_high_risk_pcr)) +
            # Low risk
            people_low_risk * prob_res_pos_given_low_risk_ag^2

          number_test_per_person <-
            (number_pcr_tests + number_alt_tests) / (people)

          total_cost_pcr <- cost_pcr * number_pcr_tests
          total_cost_alt <- cost_lamp * number_lamp_tests +
            cost_antigen * number_ag_tests
          total_cost <- total_cost_alt + total_cost_pcr


          df_out <- rbind(
            df_out,
            data.frame(
              prevalence,
              people_high_risk,
              people_low_risk,
              people_first_responder,
              sens_model = sens,
              spec_model = spec,
              sens_ag_test,
              spec_ag_test,
              sens_lamp_test,
              spec_lamp_test,
              prob_model_first_responder_pcr,
              number_alt_tests,
              number_pcr_tests,
              number_positive_reported,
              number_positive_theoretical = people * prevalence,
              number_test_per_person,
              total_cost_alt,
              total_cost_pcr,
              total_cost
            )
          )
        }
      }
    }



    return(cbind(strategy, df_out))
  }
