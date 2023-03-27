
# Analsyses on revised data
# ============================================================================ #

scores_forecasts_comparison_revised <- score_and_add_log(forecasts_comparison_revised)

p_comparison_direct_rt_individual_reivsed <- plot_rt_direct_individual(scores_forecasts_comparison_revised)

ggsave("output/figures/comparison-direct-rt-individual-revised.png",
       plot = p_comparison_direct_rt_individual,
       width = 7, height = 5)

# performance plot for an ensemble of the subset of human and direct forecasts
# using revised death data

forecasts_comparable_ensemble_revised <- forecasts_comparison_revised |>
  group_by(target_type, horizon, method, quantile, true_value, forecast_date, target_end_date) |>
  summarise(prediction = median(prediction)) |>
  mutate(model = str_to_title(method)) |>
  as.data.table()

scores_comparable_ensemble_revised <- score_and_add_log(forecasts_comparable_ensemble_revised)

p_performance_comparison_revised <- plot_performance(
  scores_comparable_ensemble_revised
)

ggsave("output/figures/performance-rt-comparison-revised.png", plot = p_performance_comparison,
       width = 7, height = 8)





  
# Analysis on a comparable ensemble of Rt and direct forecasts
# ============================================================================ #

# performance plot for an ensemble of the subset of human and direct forecasts
forecasts_comparable_ensemble <- forecasts_comparison |>
  group_by(target_type, horizon, method, quantile, true_value, forecast_date, target_end_date) |>
  summarise(prediction = median(prediction)) |>
  mutate(model = str_to_title(method)) |>
  as.data.table()

scores_comparable_ensemble <- score_and_add_log(forecasts_comparable_ensemble)

p_performance_comparison <- plot_performance(
  scores_comparable_ensemble
)

ggsave("output/figures/performance-rt-comparison.png", plot = p_performance_comparison, 
       width = 7, height = 8)

p_scores_and_pred_comparison <- plot_scores_and_pred(
  forecasts_comparable_ensemble,
  scores_comparable_ensemble
)

ggsave("output/figures/scores-and-forecasts-rt-comparison.png", 
       plot = p_scores_and_pred_comparison, 
       width = 9, height = 9)


# Only look at experts with direct forecasts
expert_ensemble_direct <- forecasts |>
  filter(model %in% models_direct) |>
  create_expert_ensemble()

expert_ensemble_direct$scores |>
  summarise_scores(c("model", "target_type", "horizon", "scale"))
make_performance_table() 

p_scores_expert_direct <- plot_performance(expert_ensemble_direct$scores)
ggsave("output/figures/performance-expert-direct.png", 
       plot = p_scores_expert_direct, 
       width = 7, height = 6)

















# Median forecasts for a 1:4 horizon
# ============================================================================ #

models <- forecasts$model |> unique()

horizon_0 <- weekly_truth |>
  filter(target_end_date >= "2021-04-01") |>
  mutate(prediction = true_value, 
         forecast_date = target_end_date + 2, 
         horizon = 0, 
         quantile = 0.5, 
         model = list(models)) |>
  tidyr::unnest(cols = model)

plot_df <- forecasts |>
  filter(quantile == 0.5) |>
  rbind(horizon_0, fill = TRUE) |>
  filter(target_end_date >= "2021-04-01", 
         target_end_date <= "2021-10-01") |>
  filter(!(model %in% c("crowd-rt", "EpiExpert-ensemble"))) |>
  filter(!(grepl("(Rt)", model))) |>
  mutate(colouring = ifelse(model == "crowd-ensemble", 
                            "Crowd ensemble", 
                            "individual particpant")) 

plot_participant_forecasts <- plot_df |>
  ggplot(aes(x = target_end_date)) +
  geom_line(aes(y = prediction, group = interaction(forecast_date, model), 
                color = "Individual participant"), 
            linewidth = .1) + 
  geom_line(data = filter(plot_df, model == "crowd-ensemble"), 
            aes(y = prediction, group = interaction(forecast_date, model), 
                color = "Crowd ensemble"),
            linewidth = .4) + 
  geom_line(aes(y = true_value),
            linewidth = .4, color = "black") + 
  geom_point(aes(y = true_value),
             size = .5, color = "black") + 
  scale_color_manual(values = c(colors[1], "grey70")) + 
  facet_wrap(~ target_type, scale = "free_y", ncol = 1) + 
  labs(x = "Date", y = "Median forecast and observed values", color = "Forecaster") + 
  theme_scoringutils()

ggsave("output/figures/participant-forecasts.png", plot = plot_participant_forecasts, 
       width = 7, height = 5)



# Median forecasts combined human ensemble and Forecast Hub ensemble 1:4 horizon
# ============================================================================ #

forecasts |>
  filter(quantile == 0.5) |>
  rbind(horizon_0, fill = TRUE) |>
  filter(target_end_date >= "2021-04-01", 
         target_end_date <= "2021-10-01") |>
  filter((model %in% c("crowd-ensemble", "EuroCOVIDhub-ensemble"))) |>
  ggplot(aes(x = target_end_date)) +
  geom_line(aes(y = prediction, group = interaction(forecast_date, model), 
                color = model), 
            linewidth = .4) + 
  geom_line(aes(y = true_value),
            linewidth = .4, color = "black") + 
  geom_point(aes(y = true_value),
             size = .5, color = "black") + 
  scale_color_manual(values = colors[1:2]) +
  facet_wrap(~ target_type, scale = "free_y", ncol = 1) + 
  labs(x = "Date", y = "Median forecast and observed values", color = "Forecaster") + 
  theme_scoringutils()




# Violin plot to compare scores
# ============================================================================ #

comparison_plot <- function(scores, models = c("crowd-direct", "EuroCOVIDhub-ensemble")) {
  mean_wis <- scores |>
    filter(scale %in% c("log", "natural")) |>
    filter(model %in% models) |>
    summarise_scores(by = c("model", "scale", "target_type", "type_and_scale")) |>
    summarise_scores(by = c("model", "scale", "target_type", "type_and_scale"),
                     fun = signif, digits = 3) |>
    mutate(wis_mean = paste("Mean:", label_fn(interval_score)))
  
  scores |>
    filter(scale %in% c("log", "natural")) |>
    filter(model %in% models) |>
    arrange(scale, target_type, model) |>
    ggplot(aes(y = interval_score, x = model)) + 
    geom_violin(aes(fill = model), color = NA,
                alpha = 0.2) + 
    geom_boxplot(alpha = 0.5) + 
    scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
    
    geom_text(data = mean_wis, 
              aes(label = wis_mean, y = -Inf), 
              vjust   = -1) + 
    facet_wrap(~ type_and_scale, scale = "free") + 
    scale_y_continuous(labels = label_fn, 
                       expand = expansion(mult = c(0.15, 0.05))) + 
    labs(y = "Weighted interval score", x = "Forecaster") + 
    theme_scoringutils() + 
    theme(legend.position = "None")
}

comparison_plot(scores, 
                models = c("crowd-direct", "EuroCOVIDhub-ensemble"))

comparison_plot(scores |>
                  filter(horizon == 2), 
                models = c("crowd-direct", "crowd-rt", "crowd-ensemble"))

