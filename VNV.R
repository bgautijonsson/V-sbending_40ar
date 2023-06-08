library(tidyverse)
library(hagstofa)
library(metill)
library(janitor)
library(clock)
library(slider)

theme_set(theme_visbending() + theme(text = element_text(family = "serif")))

vnv <- hg_data(
    "https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/1_vnv/VIS01002.px"
) |>
    collect() |>
    clean_names() |>
    rename(
        value = 4
    ) |>
    separate(manudur, into = c("ar", "man"), sep = "M", convert = T) |>
    mutate(
        dags = date_build(ar, man),
        grunnur = parse_number(grunnur)
    )

p <- vnv |>
    filter(
        visitala == "Vísitala neysluverðs"
    ) |>
    drop_na() |>
    ggplot(aes(dags, value)) +
    geom_textline(aes(group = grunnur, col = grunnur, label = grunnur)) +
    scale_x_date(
        expand = expansion()
    ) +
    scale_y_tufte(
        labels = label_tala(accuracy = 1),
        trans = "log10",
        breaks = c(10^(2:6), max(vnv$value[vnv$visitala == "Vísitala neysluverðs"], na.rm = T))
    ) +
    scale_color_gradient(
        low = "#003f4a",
        high = "#954636"
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Samanburður á vísitölum með mismunandi grunn"
    ) +
    theme(
        legend.position = "none"
    )

vnv <- vnv |>
    mutate(
        value = value / value[dags == max(dags)],
        .by = c(grunnur, visitala)
    ) |>
    summarise(
        value = mean(value, na.rm = T),
        .by = c(dags, visitala)
    ) |>
    mutate(
        breyting = c(0, diff(log(value))),
        ar_breyting = slide_dbl(breyting, sum, .before = 12, .complete = T),
        .by = visitala
    ) |>
    mutate_at(
        vars(breyting, ar_breyting),
        exp
    ) |>
    filter(
        # grunnur == 1939,
        visitala == "Vísitala neysluverðs"
    ) |>
    drop_na()

ggsave(
    plot = p,
    filename = "Myndir/VNV_grunnar.pdf",
    width = 8,
    height = 0.5 * 8,
    scale = 1.5
)

p <- vnv |>
    ggplot(aes(dags, ar_breyting)) +
    geom_hline(
        yintercept = 1,
        lty = 2,
        alpha = 0.5,
        linewidth = 0.4,
        color = "#003f4a"
    ) +
    geom_line(
        color = "#954636"
    ) +
    geom_area(
        alpha = 0.1,
        fill = "#954636"
    ) +
    scale_x_date(
        expand = expansion()
    ) +
    scale_y_tufte(
        labels = function(x) hlutf(x - 1),
        breaks = c(range(vnv$ar_breyting), 1 + 0:10/10),
        trans = "log10"
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Árleg verðbólga frá 1940"
    )

p

ggsave(
    plot = p,
    filename = "Myndir/VNV.pdf",
    width = 8,
    height = 0.5 * 8,
    scale = 1.5
)
