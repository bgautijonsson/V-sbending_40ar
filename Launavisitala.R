library(tidyverse)
library(hagstofa)
library(metill)
library(janitor)
library(clock)
library(slider)
library(geomtextpath)
library(ggtext)
library(glue)

theme_set(theme_visbending() + theme(text = element_text(family = "serif")))

col_laun <- "#32646d"

col_vnv <- "#954636"

laun <- hg_data(
    "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/launogtekjur/2_lvt/1_manadartolur/LAU04000.px"
) |>
    collect() |>
    clean_names() |>
    filter(
        eining == "Vísitölugildi"
    ) |>
    rename(value = 3) |>
    separate(manudur, into = c("ar", "man"), sep = "M", convert = TRUE) |>
    mutate(
        dags = date_build(ar, man)
    ) |>
    select(dags, value)

vlf <- hg_data(
    "https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/thjodhagsreikningar/landsframl/2_landsframleidsla_arsfj/THJ01601.px"
) |>
    collect() |>
    clean_names() |>
    rename(
        value = 4
    ) |>
    separate(arsfjordungur, c("ar", "man"), sep = "Á", convert = TRUE) |>
    mutate(
        dags = year_quarter_day(ar, man, 1) |> as_date()
    ) |>
    filter(
        skipting == "8. Verg Landsframleiðsla",
        maelikvardi == "Verðlag hvers árs"
    ) |>
    select(dags, value) |>
    mutate(
        breyting = c(0, diff(log(value))),
        ar_breyting = slide_dbl(breyting, sum, .before = 4, .complete = T) |> exp()
    ) |>
    drop_na()


plot_dat <- laun |>
    mutate(
        breyting = c(0, diff(log(value))),
        ar_breyting = slide_dbl(breyting, sum, .before = 12, .complete = T) |> exp()
    ) |>
    drop_na() |>
    select(dags, laun = ar_breyting) |>
    inner_join(
        vnv |>
            select(
                dags,
                vnv = ar_breyting
            ),
        by = join_by(dags)
    ) |>
    pivot_longer(c(vnv, laun)) |>
    drop_na()

p <- plot_dat |>
    ggplot(aes(dags, value)) +
    geom_area(
        aes(fill = name),
        position = "identity",
        alpha = 0.4
    ) +
    geom_line(
        aes(color = name)
    ) +
    scale_x_date(
        expand = expansion()
    ) +
    scale_y_tufte(
        labels = function(x) hlutf(x - 1),
        trans = "log10",
        expand = expansion()
    ) +
    scale_fill_manual(
        values = c(
            col_laun,
            col_vnv
        )
    ) +
    scale_color_manual(
        values = c(
            col_laun,
            col_vnv
        )
    ) +
    annotate(
        geom = "text",
        x = date_build(2008, 1),
        y = 1.145,
        label = "Vísitala Neysluverðs",
        color = "#954636",
        size = 5,
        angle = 83
    ) +
    annotate(
        geom = "text",
        x = date_build(2015, 4),
        y = 1.102,
        label = "Launavísitala",
        color = "#32646d",
        size = 5,
        # fontface = "bold",
        angle = 78
    ) +
    labs(
        title = glue("Árleg breyting <b style='color:{col_laun}'>launavísitölu</b> og <b style='color:{col_vnv}'>vísitölu neysluverðs</b>"),
        x = NULL,
        y = NULL
    ) +
    theme(
        legend.position = "none",
        plot.title = element_markdown()
    )


ggsave(
    plot = p,
    filename = "Myndir/laun_vs_vnv.pdf",
    width = 8,
    height = 0.621 * 8,
    scale = 1.6
)


# Uppsafnað ---------------------------------------------------------------

plot_dat <- laun |>
    select(dags, laun = value) |>
    mutate(laun = laun / 100) |>
    inner_join(
        vnv |>
            select(
                dags,
                vnv = value
            ),
        by = join_by(dags)
    ) |>
    mutate(
        vnv = vnv / vnv[dags == min(dags)]
    ) |>
    pivot_longer(c(vnv, laun)) |>
    drop_na() |>
    mutate(
        name = fct_recode(
            name,
            "Launavísitala" = "laun",
            "Vísitala neysluverðs" = "vnv"
        )
    )

p <- plot_dat |>
    ggplot(aes(dags, value)) +
    geom_area(
        aes(fill = name),
        position = "identity",
        alpha = 0.4
    ) +
    geom_labelline(
        aes(color = name, label = name),
        text_smoothing = 40,
        size = 5,
        label.padding = 0.03,
        label.r = 0,
    ) +
    scale_x_date(
        expand = expansion()
    ) +
    scale_y_tufte(
        labels = function(x) hlutf(x - 1),
        trans = "log10",
        expand = expansion(),
        breaks = c(1, max(plot_dat$value), 2:8)
    ) +
    scale_fill_manual(
        values = c(
            col_laun,
            col_vnv
        )
    ) +
    scale_color_manual(
        values = c(
            col_laun,
            col_vnv
        )
    ) +
    labs(
        title = glue("Uppsöfnuð breyting <b style='color:{col_laun}'>launavísitölu</b> og <b style='color:{col_vnv}'>vísitölu neysluverðs</b>"),
        x = NULL,
        y = NULL
    ) +
    theme(
        legend.position = "none",
        plot.title = element_markdown()
    )


ggsave(
    plot = p,
    filename = "Myndir/laun_vs_vnv_uppsafnad.pdf",
    width = 8, height = 0.5 * 8, scale = 1.6
)
