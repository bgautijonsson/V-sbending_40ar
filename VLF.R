library(tidyverse)
library(hagstofa)
library(metill)
library(janitor)
library(clock)
library(slider)
library(eurostat)
library(zoo)
library(ggthemes)
library(geomtextpath)

theme_set(theme_visbending() + theme(text = element_text(family = "serif")))

countries <- c(
    "Denmark",
    "Finland",
    "Iceland",
    "Norway",
    "Sweden"
)
icelandic <- c(
    "Denmark" = "Danmörk",
    "Finland" = "Finnland",
    "Iceland" = "Ísland",
    "Norway" = "Noregur",
    "Sweden" = "Svíþjóð"
)


country_table <- tribble(
    ~location, ~geo, ~land, ~color,
    "DNK", "Denmark", "Danmörk", "#737373",
    "FIN", "Finland", "Finnland", "#737373",
    "ISL", "Iceland", "Ísland", "#003f4a",
    "NOR", "Norway", "Noregur", "#737373",
    "SWE", "Sweden", "Svíþjóð", "#737373"
)

gdp <- read_csv("Data/DP_LIVE_08062023170656064.csv") |>
    clean_names() |>
    mutate(
        dags = date_build(time)
    ) |>
    select(
        location,
        dags,
        gdp = value
    ) |>
    inner_join(
        country_table
    )


gdp <- get_eurostat(
    "namq_10_gdp",
    cache = TRUE,
    update_cache = FALSE,
    cache_dir = "Data"
) |>
    label_eurostat() |>
    filter(
        unit == "Current prices, million euro",
        s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
        na_item == "Gross domestic product at market prices"
    ) |>
    inner_join(
        country_table
    ) |>
    select(
        dags = time,
        land,
        color,
        value = values
    )

pop <- get_eurostat(
    "demo_gind",
    cache = TRUE,
    update_cache = FALSE,
    cache_dir = "Data"
) |>
    label_eurostat() |>
    filter(
        indic_de == "Average population - total"
    ) |>
    inner_join(
        country_table
    ) |>
    select(
        dags = time,
        land,
        color,
        pop = values
    )


plot_dat <- gdp |>
    # rename(
    #     gdp = value
    # ) |>
    # mutate(dags = floor_date(dags, "year")) |>
    # summarise(
    #     gdp = mean(gdp),
    #     .by = c(dags, land, color)
    # ) |>
    left_join(
        pop |>
            select(dags, land, pop),
    ) |>
    arrange(dags) |>
    # mutate(
    #     pop = na.approx(pop, na.rm = F, maxgap = 10),
    #     .by = land
    # ) |>
    # group_by(land) |>
    # fill(pop, .direction = "down") |>
    # ungroup() |>
    mutate(
        gdp_per_cap = gdp
    ) |>
    drop_na() |>
    filter(
        year(dags) >= 1970
    )


p <- plot_dat |>
    ggplot(aes(dags, gdp_per_cap)) +
    geom_line(
        data = ~ filter(.x, land != "Ísland"),
        aes(
            group = land,
            color = color
        ),
        size = 3,
        linewidth = 0.6,
        alpha = 0.3
    ) +
    geom_textline(
        data = ~ filter(.x, land == "Ísland"),
        aes(
            label = land,
            colour = color
        ),
        hjust = 0.65,
        linewidth = 1.5,
        size = 5.5
    ) +
    geom_rangeframe(sides = "b") +
    scale_x_date(
        breaks = c(range(plot_dat$dags), date_build(c(1990, 2000, 2010))),
        labels = label_date_short()
    ) +
    scale_y_tufte(
        breaks = tufte_breaks(plot_dat$gdp_per_cap),
        labels = label_dollar(accuracy = 1, big.mark = " ", decimal.mark = ","),
        # trans = "log10"
    ) +
    scale_color_identity() +
    theme(
        legend.position = "none",
        axis.line.x = element_blank()
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Verg landsframleiðsla á mann",
        subtitle = "Á föstu verðlagi 2023. Önnur norðurlönd sýnd í gráum lit."
    )

p

ggsave(
    plot = p,
    filename = "Myndir/gdp.pdf",
    width = 8, height = 0.5 * 8, scale = 1.6
)


# GDP Change --------------------------------------------------------------

plot_dat <- plot_dat |>
    filter(
        land == "Ísland"
    ) |>
    mutate(
        change = c(1, exp(diff(log(gdp_per_cap)))),
        .by = land
    )

p <- plot_dat |>
    ggplot(aes(dags, change)) +
    geom_hline(
        yintercept = 1,
        lty = 2,
        alpha = 0.5,
        linewidth = 0.4
    ) +
    geom_area(
        fill = "#003f4a",
        alpha = 0.4
    ) +
    geom_line(
        color = "#003f4a"
    ) +
    geom_rangeframe(sides = "b") +
    scale_x_date(
        breaks = c(range(plot_dat$dags), date_build(c(1990, 2000, 2010))),
        labels = label_date_short()
    ) +
    scale_y_tufte(
        breaks = c(tufte_breaks(plot_dat$change)),
        labels = function(x) hlutf(x - 1),
        trans = "log10"
    ) +
    scale_color_identity() +
    theme(
        legend.position = "none",
        axis.line.x = element_blank()
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Hagvöxtur (% breyting í landsframleiðslu á mann)"
    )

p

ggsave(
    plot = p,
    filename = "Myndir/gdp_change.pdf",
    width = 8, height = 0.5 * 8, scale = 1.6
)
