# AWS Exploration

# 1. How do our team's average scores compare to published benchmarks?
# 2. Which scores are similar across the team, which scores vary?

# Packages
pacman::p_load(
    "worklifedata", 
    "tidyverse", 
    "ggdist"
)


# Average summary

aws_scores_long <- aws_mbi_scores |>
    select(
        matches("response_id|aws_")
    ) |>
    pivot_longer(
        matches("aws_"), 
        names_to = "scale", 
        values_to = "score"
    ) |>
    mutate(
        scale_label = scale |> str_remove("^aws_") |> str_to_title() |> fct_inorder(),
        scale_num = as.numeric(scale_label)
    )

aws_scores_long

aws_means_long <- aws_scores_long |>
    group_by(scale, scale_label, scale_num) |>
    summarize(score = mean(score))  |>
    ungroup() |>
    mutate(
        score_string = round(score, 2) |>
            stringr::str_pad(4, side = "right", pad = "0") |>
            stringr::str_replace("000", ".00"),
        score_label = case_when(
            scale_label == "Workload" ~ str_c(score_string, " (SQUAD Mean)"), 
            TRUE ~ score_string
        )
    )

aws_bench <- aws_benchmarks_quartile |>
    mutate(
        scale_label = scale |> str_remove("^aws_") |> str_to_title() |> fct_inorder(), 
        scale_num = as.numeric(scale_label), 
        value_string = round(value, 2) |>
            stringr::str_pad(4, side = "right", pad = "0") |>
            stringr::str_replace("000", ".00"),
        bench_label = case_when(
            scale_label == "Workload" ~ str_c(c("Bad (p25): ", "Middling (p50): ", "Good (p75): "), value_string), 
            TRUE ~ value_string
        )
    ) |>
    group_by(scale, scale_label) |>
    mutate(
        lwd = c(0.2, 0.5, 0.2)
    ) |>
    ungroup()

plt_aws_overview <- aws_scores_long |>
    ggplot(aes(group = scale_label, x = scale_label, y = score)) +


    ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        alpha = 0,
        point_colour = NA,
        limits = c(NA, NA)
    ) + 

    geom_rect(
        aes(
            xmin = scale_num, 
            xmax = scale_num + 0.8, 
            ymin = -Inf, 
            ymax = Inf
        ), 
        data = aws_means_long,
        colour = NA, 
        fill = "grey50", 
        alpha = 0.2
    ) + 

    geom_segment(
        aes(
            y = value, 
            yend = value, 
            x = scale_num, 
            xend = scale_num + 0.8, 
        ),
        lwd = aws_bench$lwd,
        data = aws_bench
    ) + 

    geom_text(
        aes(
            label = bench_label,
            y = value + 0.002 * max(value), 
            x = scale_num + 0.8
        ), 
        hjust = "right", 
        vjust = "bottom", 
        family = "GT Flexa",
        data = aws_bench, 
        size = 4
    ) + 

    ggdist::stat_dots(
        side = "right", 
        binwidth = 0.04, 
        shape = 21, 
        fill = NA, 
        color = "#0000FF", 
        position = position_nudge(x = 0.01)
    ) + 

    geom_segment(
        aes(
            x = scale_num, 
            xend = scale_num + 0.3, 
            yend = score
        ), 
        colour = "#0000FF", 
        data = aws_means_long
    ) + 

    geom_point(
        aes(
            x = scale_num + 0.3, 
        ), 
        shape = 21, 
        size = 3, 
        fill = "#0000FF", 
        colour = "#0000FF", 
        data = aws_means_long
    ) + 

    geom_text(
        aes(
            label = score_label, 
            x = scale_num + 0.35
        ), 
        size = 4, 
        hjust = "left",
        colour = "#0000FF", 
        family = "GT Flexa",
        data = aws_means_long
    ) + 

    scale_y_continuous(
        limits = c(1, 5), 
        expand = expansion(mult = 0.01)
    ) +

    scale_x_discrete(
        position = "top", 
        expand = c(0, 0)
    ) + 

    labs(
        x = NULL, 
        y = NULL
    ) + 

    theme_minimal() + 

    theme(
        axis.text.x.top = element_text(hjust = 0, family = "GT Flexa", size = 12), 
        axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(), 
        panel.background = element_rect(
            color = NA, 
            fill = "#e6e6e6"
        ), 
        plot.background = element_rect(
            colour = NA, 
            fill = "#e6e6e6"
        ),
        panel.grid.major = element_line(color = "grey60"), 
        panel.grid.minor = element_line(color = "grey60"),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank()
    ) 



ggsave(
    "aws-overview.png", 
    dpi = 300, 
    height = 14, 
    width = 22, 
    units = "in", 
    plot = plt_aws_overview
)
