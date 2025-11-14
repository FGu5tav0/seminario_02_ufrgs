library(tidyverse)


lei_de_bragg_d <- function(x, lambda = 1.541838, n = 1, graus = TRUE) {
  if (graus) x <- x * pi / 180  # converte para radianos
  d <- round((n * lambda) / (2 * sin((x/2))), digits = 2)
  return(d)
}

lei_de_bragg_d(1.0001)

ggplot(data.frame(x = 1:100), aes(x)) +
  stat_function(fun = lei_de_bragg_d, geom = 'line', n = 1000)




# dados drx ---------------------------------------------------------------


df <- readxl::read_excel("PowDLL_XY_XRD_Tuesday, September 2, 2025.xls") |>
  rename_with(~c("theta",paste("amostra",2:54))) |>
  pivot_longer(cols = starts_with("amostra"),
               values_to = "valor", names_to = "amostras") |>
  mutate(amostras = factor(amostras, levels = paste("amostra",2:54))) |>
  filter(theta >= 4) |>
  mutate(dist_d = lei_de_bragg_d(theta))

unique(df$dist_d)
lei_de_bragg_d(seq(5, 19, by = 2))

df |> filter(amostras %in% c("amostra 26","amostra 15",
                             "amostra 8","amostra 29",
                             "amostra 43", "amostra 44",
                             "amostra 33", "amostra 41")) |>
  mutate(grupo = case_match(amostras,
    c("amostra 26","amostra 15") ~ "S/V",
    c("amostra 8","amostra 29") ~ "N",
    c("amostra 43","amostra 44") ~ "S/V-I",
    .default = "I"
  )) |>
ggplot() +
  aes(x = theta,
      y = valor) +
  geom_line() +
  facet_wrap(~ grupo + amostras, scales = "free_y", ncol = 2,
             strip.position = "top", axes = "all_y") +
  scale_x_continuous(breaks = seq(5, 19, by = 2),
                     labels = paste0(seq(5, 19, by = 2),"\n(d ",
                                     lei_de_bragg_d(seq(5, 19, by = 2)),"\u00C5)")) +
  labs(x = "º2Theta",
       y ="Intensidade") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "gray95"))

ggsave(filename = "drx_tipo.png",
       width = 12,
       height = 8,
       dpi = 900)


