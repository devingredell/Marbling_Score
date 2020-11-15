# Packages ----------------------------------------------------------------

library(extrafont)

# Functions ---------------------------------------------------------------

lm_model <- function(y_var, dat) {
  
  formula <- as.formula(paste0(y_var, "~ group"))
  
  model <- lm(formula, dat)
  anova <- anova(model)
  emmeans <- summary(emmeans::emmeans(model, pairwise ~ group))
  
  output <- list("y_var" = y_var, "fit_model" = model,
                 "anova" = anova, "emmeans" = emmeans)
  
  return(output)
}


logit_model <- function(y_var, dat) {
  
  formula <- as.formula(paste0(y_var, "~ group"))
  
  model <- glm(formula, family = binomial(link = "logit"), dat)
  anova <- car::Anova(model)
  emmeans <- summary(emmeans::emmeans(model, pairwise ~ group, type = "response"))
  plot <- plot(emmeans::emmeans(model, pairwise ~ group, type = "response"))
  
  output <- list("fit_model" = model, "anova" = anova,
                 "emmeans" = emmeans, "plot" = plot)
  
  return(output)
}


emmeans_lm_plot <- function(y_var, dat, title = NULL) {
  
  # discrete color gradient
  discrete_gradient <- colorRampPalette(c("white", "#002453"))
  cols <- discrete_gradient(length(levels(dat$group)) + 1)[-1]
  
  # fit model and calculate emmeans
  test_model <- lm_model(y_var, dat)
  
  # add emmeans and CIs to df
  predicted <- inner_join(dat, test_model$emmeans$emmeans)
  
  # df of emmeans and superscripts for table
  means <- emmeans::emmeans(test_model$fit_model, pairwise ~ group)
  
  letters <- multcomp::cld(means$emmeans, Letters = "abcdef")
  
  letters <- tibble(letters[c("group", "emmean", ".group")]) %>%
    rename(superscript = .group) %>%
    mutate(emmean = sprintf("%.2f", round(emmean, 2)),
           group = ordered(group, levels = rev(levels(unique(dat$group)))),
           superscript = str_replace_all(superscript, " ", "")) %>%
    unite("emmean", emmean:superscript, sep = " ") %>%
    arrange(group) %>%
    rename(Average = emmean, `Marbling Group` = group)
  
  # format y_var for axis title
  x_axis_label <- str_replace_all(y_var, "_", " ")
  x_axis_label <- str_to_title(x_axis_label)
  
  # table theme (format theme using gridextra format)
  # bg_params for background and fg_params for font
  table_theme <- gridExtra::ttheme_minimal(
    colhead = list(bg_params = list(fill = "#ECF3F8", col = "#002453"),
                   fg_params = list(col = "#002453")),
    core = list(bg_params = list(fill = "white", col = "#002453"),
                fg_params = list(col = "#002453"))
  )
  
  # emmean plot with CI bars
  p <- predicted %>%
    ggplot(aes(x = .data[[y_var]], y = group, colour = group, fill = group)) +
    # geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL),
    #                size = 1, height = .5) +
    geom_point(alpha = 0) +
    geom_segment(aes(x = floor(min(predicted[y_var], na.rm = T)),
                     xend = emmean, y = group, yend = group), size = 1) +
    geom_point(aes(x = emmean, y = group, colour = group), size = 4) +
    labs(title = title,
         x = x_axis_label,
         y = NULL) +
    # caption = "Error bars indicate the 95% confidence interval.") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_y_discrete(expand = c(0, 0.2)) +
    scale_x_continuous(breaks = seq(floor(min(predicted[y_var], na.rm = T)),
                                    ceiling(max(predicted[y_var], na.rm = T)), 1),
                       minor_breaks = seq(floor(min(predicted[y_var], na.rm = T)),
                                          ceiling(max(predicted[y_var], na.rm = T)), 0.25),
                       expand = c(0,0)) +
    custom_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none")
  
  # add marginal histogram
  p_with_density <- ggExtra::ggMarginal(p, groupFill = T, margins = "x",
                                        color = "transparent", size = 3, alpha = 1)
  
  # add table
  table <- ggpubr::ggtexttable(letters, rows = NULL,
                               theme = ggpubr::ttheme(
                                 tbody.style = ggpubr::tbody_style(
                                   hjust = 0,
                                   x = 0.04,
                                   fill = "white",
                                   linecolor = "#002453",
                                   color = "#002453"),
                                 colnames.style = ggpubr::colnames_style(
                                   fill = "#ECF3F8",
                                   linecolor = "#002453",
                                   color = "#002453",
                                   face = "bold")))
  
  final_p <- ggpubr::ggarrange(p_with_density, table, ncol = 1,
                               widths = c(1, 1), heights = c(2, 1))
  
  
  output_list <- list("model_output" = test_model$fit_model, "plot" = final_p,
                      "df" = predicted)
  
  return(output_list)
  
}



emmeans_logit_plot <- function(y_var, dat, title = NULL, title_width = 50) {
  
  # discrete color gradient
  discrete_gradient <- colorRampPalette(c("white", "#002453"))
  cols <- discrete_gradient(length(levels(dat$group)) + 1)[-1]
  
  # fit model and calculate emmeans
  test_model <- logit_model(y_var, dat)
  
  
  # df of emmeans and letters for table
  probs_for_plot <- test_model$emmeans$emmeans %>%
    select(group, prob, contains("asymp")) %>%
    mutate_at(vars(prob, contains("asymp")), ~ . * 100)
  
  means <- emmeans::emmeans(test_model$fit_model, pairwise ~ group, type = "response")
  
  letters <- multcomp::cld(means$emmeans, Letters = "abcdef")
  
  letters <- tibble(letters[c("group", "prob", ".group")]) %>%
    rename(superscript = .group) %>%
    mutate(prob = sprintf("%.2f", (round(prob, 2) * 100)),
           group = ordered(group, levels = rev(levels(unique(dat$group))))) %>%
    unite("prob", prob:superscript, sep = "") %>%
    arrange(group)
  
  # format y_var for axis title
  if(y_var == "tender_cat"){
    x_axis_label <- "Probability of a Tender Strip Steak"
  } else {
    x_axis_label <- "Probability of a Positive Eating Experience"
  }
  
  
  # probs plot with CI bars
  p <- probs_for_plot %>%
    ggplot(aes(x = prob, y = group, colour = group)) +
    geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                   size = 1, height = .5) +
    geom_point(size = 4) +
    labs(title = str_wrap(title, width = title_width),
         x = x_axis_label,
         y = NULL,
         caption = "Error bars indicate the 95% CI.") +
    scale_color_manual(values = cols) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(limits = c(20, 100),
                       breaks = seq(20, 100, 10),
                       minor_breaks = seq(20, 100, 5),
                       labels = function(x) paste0(x, "%")) +
    custom_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none")
  
  # add table
  table <- ggpubr::ggtexttable(letters, rows = NULL,
                               theme = ggpubr::ttheme(
                                 tbody.style = ggpubr::tbody_style(
                                   fill = "white",
                                   linecolor = "#002453",
                                   color = "#002453",
                                   face = "bold"),
                                 colnames.style = ggpubr::colnames_style(
                                   fill = "#ECF3F8",
                                   linecolor = "#002453",
                                   color = "#002453",
                                   face = "bold")))
  
  final_p <- ggpubr::ggarrange(p, table, ncol = 2,
                               widths = c(4, 1), heights = c(3, 1))
  
  
  output_list <- list("model_output" = test_model, "plot" = final_p)
  
  return(output_list)
  
}


mean_lolipop_plot <- function(y_var, dat, title = NULL) {
  
  # discrete color gradient
  discrete_gradient <- colorRampPalette(c("white", "#002453"))
  cols <- discrete_gradient(length(levels(dat$group)) + 1)[-1]
  
  
  # format y_var for axis title
  x_axis_label <- str_replace_all(y_var, "_", " ")
  x_axis_label <- str_to_title(x_axis_label)
  
  # table theme (format theme using gridextra format)
  # bg_params for background and fg_params for font
  table_theme <- gridExtra::ttheme_minimal(
    colhead = list(bg_params = list(fill = "#ECF3F8", col = "#002453"),
                   fg_params = list(col = "#002453")),
    core = list(bg_params = list(fill = "white", col = "#002453"),
                fg_params = list(col = "#002453"))
  )
  
  # means to plot
  means <- dat %>%
    group_by(group) %>%
    mutate(mean = mean(.data[[y_var]], na.rm = T))
  
  # mean plot
  p <- means %>%
    ggplot(aes(x = .data[[y_var]], y = group, colour = group, fill = group)) +
    geom_point(alpha = 0) +
    geom_segment(aes(x = 0, xend = mean,
                     y = group, yend = group), size = 1) +
    geom_point(aes(x = mean, y = group, colour = group), size = 4) +
    labs(title = title,
         x = x_axis_label,
         y = NULL) +
    # caption = "Error bars indicate the 95% confidence interval.") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_y_discrete(expand = c(0, 0.2),
                     labels = c("Small 00",
                                "Small 50",
                                "Modest\n(Middle 1/3rd)",
                                "Top Choice\n(Upper 2/3rd)",
                                "Moderate\n(Upper 1/3rd)",
                                "Prime")) +
    scale_x_continuous(limits = c(0, 15),
                       breaks = seq(0, 15, 1),
                       expand = c(0, 0.5))  +
    custom_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none")
  
  # add marginal histogram
  p_with_density <- ggExtra::ggMarginal(p, groupFill = T, margins = "x",
                                        color = "transparent", size = 3, alpha = 1)
  
  # add table
  means_table <- dat %>%
    group_by(group) %>%
    summarise(mean = round(mean(.data[[y_var]], na.rm = T), 1)) %>%
    arrange(desc(group))
  names(means_table) <- c("Marbling Group", "Average")
  
  table <- ggpubr::ggtexttable(means_table, rows = NULL,
                               theme = ggpubr::ttheme(
                                 tbody.style = ggpubr::tbody_style(
                                   hjust = 0,
                                   x = 0.04,
                                   fill = "white",
                                   linecolor = "#002453",
                                   color = "#002453"),
                                 colnames.style = ggpubr::colnames_style(
                                   fill = "#ECF3F8",
                                   linecolor = "#002453",
                                   color = "#002453",
                                   face = "bold")))
  
  figure <- ggpubr::ggarrange(p_with_density, table,
                              ncol = 1, heights = c(2, 1))
  
  ggpubr::annotate_figure(figure,
                          fig.lab = "Data Source: Emerson et al. (2013)",
                          fig.lab.pos = "bottom.left",
                          fig.lab.size = 14)
  
  
}


mean_density_plot <- function(y_var, data, title = NULL,
                              n_iterations = 1000000,
                              prob_title = "probability",
                              footnote_text = NULL) {
  
  # custom tick labels
  if(y_var == "overall"){
    left_anchor <- "0\nNegative"
    middle_anchor <- "\nNeutral"
    right_anchor <- "15\nPositive"
    
  } else if(y_var == "juiciness"){
    left_anchor <- "0\nExtremely\nDry"
    middle_anchor <- "\nNeutral"
    right_anchor <- "15\nExtremely\nJuicy"
    
  } else if(y_var == "tenderness"){
    left_anchor <- "0\nExtremely\nTough"
    middle_anchor <- "\nNeutral"
    right_anchor <- "15\nExtremely\nTender"
    
  } else if(y_var == "shear_force"){
    left_anchor <- "0"
    middle_anchor <- " "
    right_anchor <- "15"
    
  } else{
    left_anchor <- "0\nNo Presence"
    middle_anchor <- " "
    right_anchor <- "15\nVery Strong\nPresence"
  }
  
  tick_labels <- c(left_anchor, " ")
  
  for(i in 1:6){
    tick_labels <- append(tick_labels, c(i, " "))
  }
  
  tick_labels <- append(tick_labels, c("7", middle_anchor))
  
  for(i in 8:14){
    tick_labels <- append(tick_labels, c(i, " "))
  }
  
  tick_labels <- append(tick_labels, right_anchor)
  
  
  # calculate mean and sd by group
  params <- data %>%
    #filter(!is.na({{ y_var }})) %>%
    group_by(group) %>%
    summarize(mean = mean(.data[[y_var]], na.rm = T),
              sd = sd(.data[[y_var]], na.rm = T),
              shape = fitdistrplus::fitdist(.data[[y_var]], "weibull")$estimate[[1]],
              scale = fitdistrplus::fitdist(.data[[y_var]], "weibull")$estimate[[2]])
  
  
  # format y_var for axis title
  x_axis_label <- str_replace_all(y_var, "_", " ")
  x_axis_label <- str_to_title(x_axis_label)
  
  # assign vertical line and build table based on variable
  if(y_var %in% c("overall", "tenderness")){
    vline <- 7.5
    
    # simulate weibull distribution
    simulated_data <- list()
    set.seed(13)
    for(i in seq_along(levels(params$group))){
      
      simulated_data[[i]] <- tibble(group = params[i, ]$group,
                                    value = rweibull(n_iterations,
                                                     params[i, ]$shape,
                                                     params[i, ]$scale))
    }
    
    simulated_data <- bind_rows(simulated_data)
    
    probs <- simulated_data %>%
      group_by(group) %>%
      filter(value >= 7.5) %>%
      count() %>%
      inner_join(., params) %>%
      mutate(mean = round(mean, 1),
             weib_prob = round((n / n_iterations) * 100, 0),
             weib_prob = paste0(weib_prob, "%")) %>%
      arrange(rev(group))  %>%
      select(-c(n, sd, shape, scale))
    
    names(probs) <- c("Marbling Group",
                      "Average",
                      str_wrap(prob_title, width = 20))
    
    #build prob table
    table <- ggpubr::ggtexttable(probs, rows = NULL,
                                 theme = ggpubr::ttheme(
                                   tbody.style = ggpubr::tbody_style(
                                     hjust = 0,
                                     x = 0.06,
                                     fill = "white",
                                     linecolor = "#002453",
                                     color = "#002453"),
                                   colnames.style = ggpubr::colnames_style(
                                     vjust = 0.5,
                                     y = c(0.2, 0.2, 0.5),
                                     fill = "#ECF3F8",
                                     linecolor = "#002453",
                                     color = "#002453",
                                     face = "bold"))) %>%
      ggpubr::tab_add_footnote(text = str_wrap(footnote_text, width = 30),
                               color = "#002453")
    
  } else if(y_var == "juiciness"){
    vline <- 7.5
    
    # simulate normal distribution
    simulated_data <- list()
    set.seed(13)
    for(i in seq_along(levels(params$group))){
      
      simulated_data[[i]] <- tibble(group = params[i, ]$group,
                                    value = rnorm(n_iterations,
                                                  params[i, ]$mean,
                                                  params[i, ]$sd))
    }
    
    simulated_data <- bind_rows(simulated_data)
    
    
    probs <- simulated_data %>%
      group_by(group) %>%
      filter(value >= 7.5) %>%
      count() %>%
      inner_join(., params) %>%
      mutate(mean = round(mean, 1),
             norm_prob = round((n / n_iterations) * 100, 0),
             norm_prob = paste0(norm_prob, "%")) %>%
      arrange(rev(group)) %>%
      select(-c(n, sd, shape, scale))
    
    names(probs) <- c("Marbling Group",
                      "Average",
                      str_wrap(prob_title, width = 20))
    
    #build prob table
    table <- ggpubr::ggtexttable(probs, rows = NULL,
                                 theme = ggpubr::ttheme(
                                   tbody.style = ggpubr::tbody_style(
                                     hjust = 0,
                                     x = 0.06,
                                     fill = "white",
                                     linecolor = "#002453",
                                     color = "#002453"),
                                   colnames.style = ggpubr::colnames_style(
                                     vjust = 0.5,
                                     y = c(0.2, 0.2, 0.5),
                                     fill = "#ECF3F8",
                                     linecolor = "#002453",
                                     color = "#002453",
                                     face = "bold"))) %>%
      ggpubr::tab_add_footnote(text = str_wrap(footnote_text, width = 30),
                               color = "#002453")
    
    
    
  } else{
    vline <- NULL
    
    probs <- params %>%
      mutate(mean = round(mean, 1)) %>%
      arrange(rev(group)) %>%
      select(-sd)
    
    names(probs) <- c("Marbling Group", "Average")
    
    #build prob table
    table <- ggpubr::ggtexttable(probs, rows = NULL,
                                 theme = ggpubr::ttheme(
                                   tbody.style = ggpubr::tbody_style(
                                     hjust = 0,
                                     x = 0.06,
                                     fill = "white",
                                     linecolor = "#002453",
                                     color = "#002453"),
                                   colnames.style = ggpubr::colnames_style(
                                     fill = "#ECF3F8",
                                     linecolor = "#002453",
                                     color = "#002453",
                                     face = "bold"))) %>%
      ggpubr::tab_add_footnote(text = str_wrap(footnote_text, width = 30),
                               color = "#002453")
  }
  
  
  #### create plot
  p <- simulated_data %>%
    ggplot(aes(x = value, y = group, fill = group)) +
    geom_density_ridges(quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        size = 1) +
    geom_vline(xintercept = vline, size = 1.5) +
    scale_y_discrete(expand = c(0, 0.4),
                     labels = c("Traces",
                                "Slight",
                                "Small",
                                "Modest",
                                "Moderate",
                                "Slightly\nAbundant",
                                "Moderately\nAbundant")) +
    scale_x_continuous(limits = c(0, 15),
                       breaks = seq(0, 15, 0.5),
                       minor_breaks = seq(0, 1, 0.25),
                       labels = tick_labels,
                       expand = c(.08, .08)) +
    labs(x = x_axis_label,
         y = NULL) +
    theme(legend.position = "none")
  
  
  
  #### final plot + table
  figure <- ggpubr::ggarrange(p, table, ncol = 1,
                              heights = c(2, 1))
  
  ggpubr::annotate_figure(figure,
                          fig.lab = "Data Source: Emerson et al. (2013)",
                          fig.lab.pos = "bottom.left",
                          fig.lab.size = 14)
  
}

# ggplot Theme ------------------------------------------------------------

# custom theme colors
cols <- c("#F3AF00", "#002453", "#916600", "#809E9B", "#506B67", "#FD3C31", "#B02D2A")

# options(ggplot2.discrete.colour = cols,
#         ggplot2.distrete.fill = cols)

custom_theme <- function(){
  
  theme_minimal() +
    theme(
      text = element_text(color = "#002453",
                          face = "plain", size = 18),
      plot.title = element_text(size = 25, color = "#002453"),
      strip.text = element_text(color = "#002453",
                                size = 15,  hjust = 0.5),
      strip.background = element_rect(fill = "#ECF3F8", color = NA),
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      axis.text = element_text(color = "#002453",
                               face = "plain", size = 15),
      legend.title = element_blank(),
      legend.text = element_text(size = 15, color = "#74787B"),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}
