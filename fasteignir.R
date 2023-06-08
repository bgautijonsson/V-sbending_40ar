library(tidyverse)
# library(hagstofa)
library(metill)
library(janitor)
library(clock)
library(slider)
library(eurostat)
library(zoo)
library(ggthemes)
library(geomtextpath)
library(rjson)
library(arrow)
library(duckdb)
library(patchwork)

theme_set(theme_visbending() + theme(text = element_text(family = "serif")))

main_color <- "#003f4a"

subtitle_size <- 12

get_tables <- function(data, years) {
    tibble(
        ar = years,
        svfn = data$svfn,
        sveitarfelag = data$name,
        fjolbyli_fjoldi = data$fjolbyli_fjoldi,
        fjolbyli_medaltal = data$fjolbyli_medaltal,
        fjolbyli_midgildi = data$fjolbyli_midgildi,
        serbyli_fjoldi = data$serbyli_fjoldi,
        serbyli_medaltal = data$serbyli_medaltal,
        serbyli_midgildi = data$serbyli_midgildi,
    ) |>
        pivot_longer(c(-svfn, -sveitarfelag, -ar), names_to = c("tegund", "breyta"), names_sep = "_") |>
        mutate(value = ifelse(value == "None", "0", value)) |>
        pivot_wider(names_from = breyta, values_from = value)
}

init_db()

d <- fromJSON(file = "https://talnaefni.fasteignaskra.is/talnaefni/v1/staerdibudasveitarfelog")

d <- d$sveitarfélög |>
    map(get_tables, years = d$date) |>
    reduce(bind_rows) |>
    mutate_at(vars(ar, svfn, fjoldi, medaltal, midgildi), parse_number) |>
    select(ar, sveitarfelag, tegund, fjoldi)

d <- d |>
    count(ar, wt = fjoldi, name = "fjoldi") |>
    arrange(ar) |>
    mutate(cum_fjoldi = cumsum(fjoldi))

mannfjoldi <- mtl_mannfjoldi() |>
    mutate(
        arnar = ifelse(aldur == 29, 1, 0),
        fullordin = ifelse(aldur >= 20, 1, 0)
    ) |>
    group_by(ar) |>
    summarise(
        pop_fullordin = sum(pop * fullordin),
        pop_arnar = sum(pop * arnar),
        pop_total = sum(pop)
    ) |>
    dplyr::collect()


d <- d |>
    left_join(
        mannfjoldi,
        by = c("ar")
    ) |>
    fill(pop_fullordin, pop_arnar, pop_total, .direction = "down") |>
    drop_na()



plot_dat <- d

p1 <- plot_dat |>
    ggplot(aes(ar, fjoldi), col = main_color) +
    geom_area(
        fill = main_color,
        alpha = 0.3
    ) +
    geom_line(col = main_color) +
    scale_x_tufte(
        breaks = tufte_breaks(plot_dat$ar)
    ) +
    scale_y_tufte(
        breaks = tufte_breaks(plot_dat$fjoldi),
        labels = label_number(big.mark = ".", decimal.mark = ","),
        expand = expansion()
    ) +
    labs(
        x = NULL,
        y = NULL,
        subtitle = "Fjöldi nýrra fasteigna"
    ) +
    theme(
        plot.subtitle = element_text(size = subtitle_size)
    )

p1

plot_dat <- d |>
    arrange(ar) |>
    mutate(diff = c(NA, diff(pop_total)))


p2 <- plot_dat |>
    ggplot(aes(ar, pop_total)) +
    geom_area(
        fill = main_color,
        alpha = 0.3
    ) +
    geom_line(col = main_color) +
    scale_x_tufte(
        breaks = tufte_breaks(plot_dat$ar)
    ) +
    scale_y_tufte(
        breaks = c(0, tufte_breaks(plot_dat$pop_total)),
        labels = label_number(big.mark = ".", decimal.mark = ","),
        expand = expansion()
    ) +
    labs(
        x = NULL,
        y = NULL,
        subtitle = "Mannfjöldi á Íslandi"
    ) +
    theme(
        plot.subtitle = element_text(size = subtitle_size)
    )

p2

plot_dat <- d |>
    mutate(p = fjoldi / pop_total * 1000)


p3 <- plot_dat |>
    ggplot(aes(ar, p)) +
    geom_area(
        fill = main_color,
        alpha = 0.3
    ) +
    geom_line(col = main_color) +
    scale_x_tufte(
        breaks = tufte_breaks(plot_dat$ar)
    ) +
    scale_y_tufte(
        breaks = tufte_breaks(plot_dat$p),
        labels = label_number(accuracy = 1, big.mark = ".", decimal.mark = ","),
        expand = expansion()
    ) +
    labs(
        x = NULL,
        y = NULL,
        subtitle = "Nýjar fasteignir á hverja þúsund íbúa"
    ) +
    theme(
        plot.subtitle = element_text(size = subtitle_size)
    )

p3

p <- p1 + p2 + p3 +
    plot_annotation(
        title = "Fasteignauppbygging og mannfjöldi",
        theme = theme(
            plot.margin = margin(t = 5, r = 35, b = 5, l = 35)
        )
    )

p

ggsave(
    plot = p,
    filename = "Myndir/pop_total.pdf",
    width = 8, height = 0.3 * 8, scale = 1.6
)


plot_dat <- d |>
    mutate(p = pop_fullordin / pop_total)


p1 <- plot_dat |>
    ggplot(aes(ar, p)) +
    geom_area(
        fill = main_color,
        alpha = 0.3
    ) +
    geom_line(col = main_color) +
    scale_x_tufte(
        breaks = tufte_breaks(plot_dat$ar)
    ) +
    scale_y_tufte(
        breaks = c(0.561, 0.66, 0.753),
        labels = label_percent(accuracy = 1, big.mark = ".", decimal.mark = ","),
        expand = expansion(),
        limits = c(0, 1)
    ) +
    labs(
        x = NULL,
        y = NULL,
        subtitle = "Hlutfall fullorðinna af íbúum"
    ) +
    theme(
        plot.subtitle = element_text(size = subtitle_size)
    )

p1


plot_dat <- d |>
    mutate(p = fjoldi / pop_fullordin * 1000)


p2 <- plot_dat |>
    ggplot(aes(ar, p)) +
    geom_area(
        fill = main_color,
        alpha = 0.3
    ) +
    geom_line(col = main_color) +
    scale_x_tufte(
        breaks = tufte_breaks(plot_dat$ar)
    ) +
    scale_y_tufte(
        breaks = tufte_breaks(plot_dat$p),
        labels = label_number(accuracy = 1, big.mark = ".", decimal.mark = ","),
        expand = expansion()
    ) +
    labs(
        x = NULL,
        y = NULL,
        subtitle = "Nýjar fasteignir á hverja þúsund fullorðna"
    ) +
    theme(
        plot.subtitle = element_text(size = subtitle_size)
    )

p <- p1 + p2 +
    plot_annotation(
        title = "Fullorðnum fjölgar hraðar en börnum"
    )

ggsave(
    plot = p,
    filename = "Myndir/pop_fullordin.pdf",
    width = 8, height = 0.4 * 8, scale = 1.5
)
