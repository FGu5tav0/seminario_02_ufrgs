library(gt)
library(gtExtras)
library(patchwork)
library(tidyverse)

# Função para gerar o gráfico e salvá-lo
generate_plot <- function(shape, col, fill, manu) {
  plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point(aes(shape = factor(shape)),col = col,fill = fill, size = 10) +
    scale_shape_manual(values = shape) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(t = -100,r = -100,b = -100,l = -100,unit = "mm"))

  # Salva o gráfico como imagem
  filename <- paste0("shapes/", manu, ".png")
  ggsave(filename, plot = plot, width = 1, height = 1, scale = .5)
  return(filename)
}


cores <- c(
  "#d62728", "#ff7f0e", "#bcbd22","#2ca02c","#7f7f7f","#9467bd",
  "#e377c2", "#1f77b4", "#8c564b", "#98df8a", "gold")

shapes <- rep(22,11)

rotulos <- c(
  "CQFS-RS/SC (2016)",
  "Pauletti and Motta (2019)",
  "Cantarella et al. (2022)",
  "Ribeiro et al. (1999)",
  "Freire et al. (2013)",
  "Zancanaro et al. (2022)",
  "Brasil et al. (2020)",
  "Sousa and Lobato (2004)",
  "Cubilla et al. (2012)",
  "Fontoura et al. (2015)",
  "Fiorin et al. (2024)"
)


#
manual <- paste0(1:11,"-manu")
#
for (i in 1:length(shapes)) {
  generate_plot(shape = shapes[i], col = "black", fill = cores[i],
                manu = manual[i])
}

files <- list.files(path = "shapes/", full.names = T)

dft <- files |> as.data.frame()

dft <- dft |> mutate(ordem = str_extract(files, "\\d+"),
              ordem = as.integer(ordem)) |>
  arrange(ordem) |>
  mutate(sistema = rotulos)


# dados de fora -----------------------------------------------------------

dados <- tribble(
  ~`Sistema de recomendação`, ~Local, ~`Camada diagnóstica (cm)`, ~Extrator, ~Grupo,
  "CQFS-RS/SC (2016)",        "RS/SC", "0-10", "Mehlich-I", "Oficiais",
  "Pauletti and Motta (2019)", "PR",    "0-20", "Mehlich-I", "Oficiais",
  "Cantarella et al. (2022)",  "SP",    "0-20", "Resina",    "Oficiais",
  "Ribeiro et al. (1999)",     "MG",    "0-20", "Mehlich-I", "Oficiais",
  "Freire et al. (2013)",      "RJ",    "0-20", "Mehlich-I", "Oficiais",
  "Zancanaro et al. (2022)",   "MT",    "0-20", "Mehlich-I", "Oficiais",
  "Brasil et al. (2020)",      "PA",    "0-20", "Mehlich-I", "Oficiais",
  "Sousa and Lobato (2004)",   "Cerrado", "0-20", "Mehlich-I", "Oficiais",
  "Cubilla et al. (2012)",     "Py",    "0-10", "Mehlich-I", "Oficiais",
  "Fontoura et al. (2015)",    "PR",    "0-20", "Mehlich-I", "Regionais",
  "Fiorin et al. (2024)",      "RS",    "0-20", "Mehlich-I", "Regionais"
)

base <- full_join(dft, dados, by = join_by("sistema" == `Sistema de recomendação`)) |>
  select(-ordem)






base |>
  gt(groupname_col = "Grupo") %>%
  tab_header(
    title = md("**Sistemas de recomendação**")
  ) |>
  text_transform(
    locations = cells_body(columns = files),
    fn = function(x) {
      local_image(
        filename = base$files,
        height = 20
      )
    }
  ) |>
  cols_label(
    files = "",
    `Camada diagnóstica (cm)` = html("Camada<br>diagnóstica (cm)"),
    sistema = "Sistema"
  ) |>
  tab_style(
    style = list(cell_text(color = "red", weight = "bold")),
    locations = cells_body(
      columns = Extrator,
      rows = Extrator == "Resina"
    )
  ) %>%
  # Destacar '0-10' em vermelho
  tab_style(
    style = list(cell_text(color = "red", weight = "bold")),
    locations = cells_body(
      columns = `Camada diagnóstica (cm)`,
      rows = `Camada diagnóstica (cm)` == "0-10"
    )
  ) |>
  tab_footnote(
    footnote = md("$${K_{M1}}=1.77K_{(Resina)}$$ (Bortolon, 2011)"),
    locations = cells_body(
      columns = Extrator,
      rows = Extrator == "Resina"
    )
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_style(
    style = list(
      cell_text(size = px(18), weight = "bold")  # tamanho da fonte em px e negrito
    ),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_options(
    footnotes.marks = "*",
    footnotes.font.size = px(10),
    # table.width = px(200),
    table.font.size = px(18),  # Ajustar o tamanho da fonte
    # heading.font.size = px(12),  # Ajustar o tamanho da fonte do cabeçalho
    column_labels.font.size = px(13),  # Ajustar o tamanho da fonte dos rótulos das colunas
    row.striping.include_table_body = FALSE,  # Desativar a listragem para reduzir espaçamento visual
    table.border.top.width = px(0.01),  # Reduzir a largura da borda superior da tabela
    # table.border.top.color = "black",
    table.border.bottom.width = px(0.5),  # Reduzir a largura da borda inferior da tabela
    column_labels.border.bottom.width = px(1),  # Reduzir a largura da borda inferior dos rótulos das colunas
    # row.padding = px(2),  # Reduzir o espaçamento vertical entre linhas
    column_labels.padding = px(2),
    data_row.padding = px(3)
  ) -> gt_table


gt_table |>
  gtsave("tab_2.html")


# grafico_total_soja + wrap_table(gt_table)

