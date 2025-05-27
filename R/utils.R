#! Plot functions
#! --------------- 

scatter_plot_nse_kge_c2mp <- function(data, metric, mode, model_x, cal_method_x, model_y, cal_method_y) {

  data <- data |>
    dplyr::filter(Mode == mode) |> 
    dplyr::filter(
      (cal_method == cal_method_x & Variable == model_x) | (cal_method == cal_method_y & Variable == model_y)
    ) |> 
    dplyr::select(Code, TimeC, Hprev, Mask, cal_method, {{ metric }}, Variable) |> 
    tidyr::pivot_wider(
        names_from = c(Variable, cal_method),
        values_from = {{ metric }}
    )
  
  colname_x <- paste(model_x, cal_method_x, sep = "_")
  colname_y <- paste(model_y, cal_method_y, sep = "_")

  count <- data |> 
    dplyr::mutate(
      compare_model = dplyr::case_when(
        .data[[colname_y]] > .data[[colname_x]] ~ -1,
        .data[[colname_y]] < .data[[colname_x]] ~ 1,
        .data[[colname_y]] == .data[[colname_x]] ~ 0
      )
    ) |> 
    dplyr::group_by(Hprev, Mask, compare_model) |> 
    dplyr::summarise(n = n(), .groups = "drop")

  p <- data |> 
    ggplot2::ggplot(ggplot2::aes(x = !!sym(colname_x), y = !!sym(colname_y))) +
    ggplot2::geom_point(aes(color = TimeC), alpha = 0.25) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    ggplot2::facet_grid(Mask~Hprev) +
      ggplot2::labs(
        title = paste0(metric, " borné [-]"),
        x = paste0(model_x, " (", cal_method_x, ")"),
        y = paste0(model_y, " (", cal_method_y, ")"),
        color = "Temps de réponse"
      )
  
  if (metric != "C2MP") {
    p <- p +
      ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::geom_text(
        data = count, 
        ggplot2::aes(
          x = dplyr::case_when(
            compare_model == -1 ~ 0.15, 
            compare_model == 1  ~ 0.85,
            compare_model == 0  ~ 0 
          ),
          y = case_when(
            compare_model == -1 ~ 0.85, 
            compare_model == 1  ~ 0.15,
            compare_model == 0  ~ 0 
          ),
          label = paste0("n = ", n),
          color = factor(compare_model)
        ),
        size = 3
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "-1" = "black", 
          "1" = "black", 
          "0" = "black",
          "Tr [1, 9] h" = "#377EB8",
          "Tr [10, 18] h" = "#E41A1C",
          "Tr [19, 128] h" = "#4DAF4A"
        ),
        breaks = c("Tr [1, 9] h", "Tr [10, 18] h", "Tr [19, 128] h")
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        panel.spacing = ggplot2::unit(0.3, "cm")
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  } else {
    p <- p +
      ggplot2::coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
      ggplot2::geom_text(
        data = count, 
        ggplot2::aes(
          x = dplyr::case_when(
            compare_model == -1 ~ -0.75, 
            compare_model == 1  ~ 0.75,
            compare_model == 0  ~ 0 
          ),
          y = case_when(
            compare_model == -1 ~ 0.85, 
            compare_model == 1  ~ -0.85,
            compare_model == 0  ~ 0 
          ),
          label = paste0("n = ", n),
          color = factor(compare_model)
        ),
        size = 3
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "-1" = "black", 
          "1" = "black", 
          "0" = "black",
          "Tr [1, 9] h" = "#377EB8",
          "Tr [10, 18] h" = "#E41A1C",
          "Tr [19, 128] h" = "#4DAF4A"
        ),
        breaks = c("Tr [1, 9] h", "Tr [10, 18] h", "Tr [19, 128] h")
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom"
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

  }
  
  return(p)
}

#! Scatter plot (contingency table)
scatter_plot_cont_table <- function(data, mode, model_x, cal_method_x, model_y, cal_method_y) {

  data <- data |>
    dplyr::filter(Mode == mode) |> 
    dplyr::filter(
      (cal_method == cal_method_x & Variable == model_x) | (cal_method == cal_method_y & Variable == model_y)
    ) |> 
    dplyr::select(Code, TimeC, Hprev, Metric, cal_method, Variable, Value) |> 
    tidyr::pivot_wider(
        names_from = c(Variable, cal_method),
        values_from = Value
    )
  
  colname_x <- paste(model_x, cal_method_x, sep = "_")
  colname_y <- paste(model_y, cal_method_y, sep = "_")

  count <- data |> 
    dplyr::mutate(
      compare_model = dplyr::case_when(
        .data[[colname_y]] > .data[[colname_x]] ~ -1,
        .data[[colname_y]] < .data[[colname_x]] ~ 1,
        .data[[colname_y]] == .data[[colname_x]] ~ 0
      )
    ) |> 
    dplyr::group_by(Hprev, Metric, compare_model) |> 
    dplyr::summarise(n = n(), .groups = "drop")

  p <- data|> 
    ggplot2::ggplot(ggplot2::aes(x = !!sym(colname_x) , y = !!sym(colname_y))) +
    ggplot2::geom_point(aes(color = TimeC), size = 1, alpha = 0.25) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    ggplot2::facet_grid(Metric~Hprev) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1) 
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(scale = 1) 
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Tableau de contingence",
      x = paste0(model_x, " (", cal_method_x, ")"),
      y = paste0(model_y, " (", cal_method_y, ")"),
      color = "Temps de réponse"
    ) +
    ggplot2::geom_text(
      data = count, 
      ggplot2::aes(
        x = dplyr::case_when(
          compare_model == -1 ~ 15, 
          compare_model == 1  ~ 85,  
          compare_model == 0  ~ 10 
        ),
        y = case_when(
          compare_model == -1 ~ 85, 
          compare_model == 1  ~ 15,
          compare_model == 0  ~ 35 
        ),
        label = paste0("n = ", n),
        color = factor(compare_model)
      ),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "-1" = "black", 
        "1" = "black", 
        "0" = "black",
        "Tr [1, 9] h" = "#377EB8",
        "Tr [10, 18] h" = "#E41A1C",
        "Tr [19, 128] h" = "#4DAF4A"
      ),
      breaks = c("Tr [1, 9] h", "Tr [10, 18] h", "Tr [19, 128] h")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.spacing = ggplot2::unit(0.3, "cm"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

  return(p)
}

#! Boxplot (NSE, KGE) for a specified calibration method
boxplot_nge_kge_c2mp <- function(data, metric, method) {
  p <- data |> 
    dplyr::filter(cal_method == method) |> 
    dplyr::select(Code, Hprev, Mask, {{ metric }}, Variable, Mode) |> 
    dplyr::group_by(Variable, Hprev, Mask, Mode) |> 
    dplyr::summarise(
      Q05 = quantile(!!sym(metric), 0.05, na.rm = TRUE),
      Q25 = quantile(!!sym(metric), 0.25, na.rm = TRUE),
      Q50 = quantile(!!sym(metric), 0.50, na.rm = TRUE),
      Q75 = quantile(!!sym(metric), 0.75, na.rm = TRUE),
      Q95 = quantile(!!sym(metric), 0.95, na.rm = TRUE)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = Variable, fill = Mode)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        ymin = Q05, lower = Q25, middle = Q50, upper = Q75, ymax = Q95
      ), 
      alpha = 0.5, 
      stat = 'identity'
    ) +
    ggplot2::facet_grid(Hprev ~ Mask, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = paste0(metric, " borné [-]"),
      fill = "Mode des résultats",
      color = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Calage" = "#1B9E77",
        "Évaluation"= "mediumorchid"
      )
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.y = ggplot2::unit(0.3, "cm")
    )
  
  return(p)
}

#! Boxplot (NSE, KGE) comparing all calibration methods (evaluation mode)
boxplot_nse_kge_c2mp_cal_method <- function(data, mode, metric) {
  p <- data |> 
    dplyr::filter(Mode == mode) |> 
    dplyr::select(Code, Hprev, Mask, cal_method, {{ metric }}, Variable) |> 
    dplyr::group_by(Variable, Hprev, cal_method, Mask) |> 
    dplyr::summarise(
      Q05 = quantile(!!sym(metric), 0.05, na.rm = TRUE),
      Q25 = quantile(!!sym(metric), 0.25, na.rm = TRUE),
      Q50 = quantile(!!sym(metric), 0.50, na.rm = TRUE),
      Q75 = quantile(!!sym(metric), 0.75, na.rm = TRUE),
      Q95 = quantile(!!sym(metric), 0.95, na.rm = TRUE)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = Variable, fill = cal_method)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        ymin = Q05, lower = Q25, middle = Q50, upper = Q75, ymax = Q95
      ), 
      alpha = 0.5, 
      stat = 'identity'
    ) +
    ggplot2::facet_grid(Hprev ~ Mask, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = paste0(metric, " borné [-]"),
      fill = "Méthode de calage",
      color = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "WsRf" = "#60a9c9",
        "Ref"= "#e7646f",
        "OL" = "#f4d261"
      )
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.y = ggplot2::unit(0.3, "cm")
    )
  
  return(p)
}

#! Boxplot CT results
#! ------------------
boxplot_cont_table <- function(data, mode, method) {
  p <- data |> 
    dplyr::filter(Mode == mode) |> 
    dplyr::filter(cal_method == method) |> 
    dplyr::group_by(Variable, Hprev, Metric) |> 
    dplyr::summarise(
      Q05 = quantile(Value, 0.05, na.rm = TRUE),
      Q25 = quantile(Value, 0.25, na.rm = TRUE),
      Q50 = quantile(Value, 0.50, na.rm = TRUE),
      Q75 = quantile(Value, 0.75, na.rm = TRUE),
      Q95 = quantile(Value, 0.95, na.rm = TRUE)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = Variable, fill = Variable)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        ymin = Q05, lower = Q25, middle = Q50, upper = Q75, ymax = Q95
      ), 
      alpha = 0.5, 
      stat = 'identity'
    ) +
    ggplot2::facet_grid(Metric ~ Hprev, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      color = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1) 
    ) + 
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

#! Boxplot (NSE, KGE) for different response time
boxplot_tr_cal_method <- function(data, metric, mode, method, mask) {

  boxplot_tr <- function(metric, temps_reponse, method, show_strip) {
    p <- data |> 
      dplyr::filter(Mode == mode) |> 
      dplyr::filter(cal_method == method) |> 
      dplyr::filter(Mask == mask) |> 
      dplyr::filter(TimeC == temps_reponse) |> 
      dplyr::group_by(Variable, Hprev, TimeC) |> 
      dplyr::summarise(
        Q05 = quantile(!!sym(metric), 0.05, na.rm = TRUE),
        Q25 = quantile(!!sym(metric), 0.25, na.rm = TRUE),
        Q50 = quantile(!!sym(metric), 0.50, na.rm = TRUE),
        Q75 = quantile(!!sym(metric), 0.75, na.rm = TRUE),
        Q95 = quantile(!!sym(metric), 0.95, na.rm = TRUE)
      ) |> 
      ggplot2::ggplot(ggplot2::aes(y = factor(Variable, levels = rev(levels(Variable))), fill = Variable)) +
      ggplot2::geom_boxplot(
        ggplot2::aes(
          xmin = Q05, xlower = Q25, xmiddle = Q50, xupper = Q75, xmax = Q95,
          group = interaction(Variable, Hprev)
        ), 
        alpha = 0.5, 
        stat = 'identity'
      ) +
      ggplot2::facet_grid(TimeC ~ Hprev, scales = "free") +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = NULL,
        color = NULL
      ) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        strip.text.x = if (!show_strip) ggplot2::element_blank() else ggplot2::element_text(),
        plot.margin = ggplot2::unit(c(0.3, 0, 0, 0), "cm"),
        panel.spacing.x = ggplot2::unit(0.5, "cm")
      )
  }

  p1 <- boxplot_tr(metric, "Tr [1, 9] h", method,  show_strip = TRUE) + ggplot2::labs(title = paste0(metric, " borné [-]"))
  p2 <- boxplot_tr(metric, "Tr [10, 18] h", method, show_strip = FALSE)
  p3 <- boxplot_tr(metric, "Tr [19, 128] h", method, show_strip = FALSE)


  p <- (p1 / p2 / p3)

  return(p)
}

#! Boxplot for catchments with a specified class of response time (in evaluation mode) to compare performances WsRf and Ref calibration methods
boxplot_tr_long_WsRf_OL <- function(data, mode, metric, mask, Tr) {
  p <- data |> 
    dplyr::filter(Mode == mode) |> 
    dplyr::filter(Mask == mask) |> 
    dplyr::filter(TimeC == Tr) |> 
    dplyr::filter(cal_method != "OL") |> 
    dplyr::group_by(Variable, Hprev, cal_method) |> 
    dplyr::summarise(
      Q05 = quantile(!!sym(metric), 0.05, na.rm = TRUE),
      Q25 = quantile(!!sym(metric), 0.25, na.rm = TRUE),
      Q50 = quantile(!!sym(metric), 0.50, na.rm = TRUE),
      Q75 = quantile(!!sym(metric), 0.75, na.rm = TRUE),
      Q95 = quantile(!!sym(metric), 0.95, na.rm = TRUE)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(y = factor(Variable, levels = rev(levels(Variable))), fill = cal_method)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        xmin = Q05, xlower = Q25, xmiddle = Q50, xupper = Q75, xmax = Q95,
        group = interaction(Variable, Hprev, cal_method)
      ), 
      alpha = 0.5, 
      stat = 'identity'
    ) +
    ggplot2::facet_wrap(~Hprev, scales = "free_x") +
    ggplot2::labs(
      title = paste0("Temps de réponse : ", Tr),
      x = NULL,
      y = NULL,
      fill = NULL,
      color = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "WsRf" = "#60a9c9",
        "Ref" = "#e7646f"
      ), 
      breaks = c("WsRf", "Ref")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )

  return(p)
}

