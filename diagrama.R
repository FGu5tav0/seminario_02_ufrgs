library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

diagrama <- grViz("
digraph calagem_adubacao {

  graph [layout = dot, rankdir = LR, nodesep = 0.2]

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12, width = 3]

  # Coluna azul (usuário)
  user      [label = 'Usuário', fillcolor = '#505d7c', fontcolor = 'white']
  amostra_u [label = 'Amostragem', fillcolor = '#5DADE2']
  lab       [label = 'Análise em laboratório', fillcolor = '#5DADE2']
  interpret [label = 'Interpretação dos resultados', fillcolor = '#5DADE2']
  recomend  [label = 'Recomendação', fillcolor = '#5DADE2']

  # Coluna verde (pesquisa)
  pesquisa    [label = 'Pesquisa', fillcolor = '#505d7c', fontcolor = 'white']
  amostra_p   [label = 'Amostragem', fillcolor = '#58D68D']
  selecao     [label = 'Seleção de métodos', fillcolor = '#58D68D']
  calibracao  [label = 'Calibração dos métodos', fillcolor = '#58D68D']
  resposta    [label = 'Resposta à adubação', fillcolor = '#58D68D']
  sistema     [label = 'Sistema de recomendação', fillcolor = '#58D68D']

  # banco dados
  dados1 [label = 'Dados 1', fillcolor = '#ccff41', fontcolor = 'black', width = 0.9]
  dados2 [label = 'Dados 2', fillcolor = '#ccff41', fontcolor = 'black', width = 0.9]

  # Manual
  manual [label = 'Manual de Calagem e Adubação', shape = box, style = filled, fillcolor = orange, fontcolor = white, fontsize = 14]

  # Posicionamento lado a lado
  { rank = same; pesquisa; user}
  { rank = same; amostra_u; amostra_p }
  { rank = same; lab; selecao }
  { rank = same; interpret; calibracao }
  { rank = same; recomend; resposta }
  # { rank = same; manual; sistema }

  # Caminho azul
  user      -> amostra_u -> lab -> interpret -> recomend
  # Caminho verde
  pesquisa  -> amostra_p -> selecao -> calibracao -> resposta -> sistema -> manual

  # Retorno do manual ao usuário
  manual -> user [color=navy, penwidth=2]

  # user para dados
  # user -> dados1 [style=dotted]
  amostra_u -> dados1 [style=dotted color=red]
  lab -> dados1 [style=dotted color=red]
  interpret -> dados1 [style=dotted color=red]
  recomend -> dados1 [style=dotted color=red]
  amostra_p -> dados2 [style=dotted color=red]
  selecao -> dados2 [style=dotted color=red]
  calibracao -> dados2 [style=dotted color=red]
  resposta -> dados2 [style=dotted color=red]
  sistema -> dados2 [style=dotted color=red]

  # dados para pesquisa
  dados1 -> pesquisa [style=dotted color=red]
  dados2 -> pesquisa [style=dotted color=red]

}
")

# Exportar para SVG (string SVG)
svg_xml <- export_svg(diagrama)
# diagrama
# Salvar como arquivo SVG
writeLines(svg_xml, "meu_diagrama.svg")



# gráfico  ----------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(dplyr)
library(patchwork)


# Criar os dados
fertility_data <- data.frame(
  Classe = c("E. Muito alto", "D. Alto", "C. Médio", "B. Baixo", "A. Muito baixo"),
  Recomendação = c("Sem fertilização",
                     "Dose < K_exportado pela cultura",
                     "Dose = K_exportado pela cultura",
                     "Dose > K_exportado pela cultura",
                     "Dose >> K_exportado pela cultura")
)

# Criar a tabela como um gráfico
table_plot <- ggplot() +
  theme_void() +
  annotation_custom(tableGrob(fertility_data,
                              rows = NULL,
                              theme = ttheme_minimal(
                                base_size = 10,
                                padding = unit(c(4, 4), "mm"),
                                core = list(
                                  fg_params = list(hjust = 0, x = 0.05)
                                )
                              ))) +
  labs(title = "Classe de fertilidade e recomendação") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -5, face = "bold", size = 10, margin = margin(t = 12, unit = "mm")))

# table_plot



# Função para simular as curvas
generate_curve <- function(time, class, a = 4, b = 2, k = 0.3) {
  y <- case_when(
    class == "A" ~ 0.5 - a * exp(-k * time),  # muito abaixo
    class == "B" ~ 1 - b * exp(-k * time),  # abaixo
    class == "C" ~ rep(1.5, length(time)),    # correto
    class == "D" ~ 2 + b * exp(-k * time),  # acima
    class == "E" ~ 2.5 + a * exp(-k * time)   # muito acima
  )
  data.frame(time = time, y = y, class = class)
}
# Gerar as curvas
time_vals <- seq(0, 10, length.out = 100)
curves <- bind_rows(
  generate_curve(time_vals, "A"),
  generate_curve(time_vals, "B"),
  generate_curve(time_vals, "C"),
  generate_curve(time_vals, "D"),
  generate_curve(time_vals, "E")
)

ya<- curves |> group_by(class) |>  slice(1) |> select(-time, - class)
# Rótulos ao lado direito (no fim da curva)
label_positions <- curves %>%
  group_by(class) %>%
  slice_tail(n = 1)

label_positions$y2 <- ya$y


# Plot
ggplot(curves, aes(x = time, y = y, group = class)) +
  geom_path(arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
            size = 0.5) +
  geom_text(data = label_positions, aes(x = 0, y = y2, label = class),
              hjust = -0.3, vjust = 0.5, size = 3, nudge_y = 1) +
  labs(x = "Tempo (anos)", y = NULL,) +
  theme_minimal(base_size = 10) +
  theme(
    axis.line.x = element_line(),
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    aspect.ratio = 0.7
  ) +
  coord_cartesian(xlim = c(0, 10), clip = "off") -> curva_plot

# final_plot | (plot_spacer() / curva_plot / plot_spacer()) +
table_plot | curva_plot +
  plot_layout(widths = c(0.7, 0.3))

ggsave(filename = "teste_clases_curva.jpeg",
       width = 7, height = 4, dpi = 600)


# -------------------------------------------------------------------------

library(DiagrammeR)

grViz("
digraph nivel_critico {

  graph [layout = neato, overlap = false]

  node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica]

  # Nós periféricos
  caltura     [label = 'Cultura']
  profundidade [label = 'Profundidade de coleta']
  metodo      [label = 'Método de extração']
  n_exp       [label = 'N experimental']
  epoca       [label = 'Época de coleta']
  definicao   [label = 'Definição de rendimento relativo']
  meta        [label = 'Meta de rendimento relativo']
  modelo      [label = 'Modelo de ajuste']
  solo        [label = 'Tipo de solo']
  tempo        [label = 'Tempo']

  # Nó central
  nivel       [label = 'Nível crítico', shape = ellipse, fillcolor = orange]

  # Conexões para o centro
  caltura     -> nivel
  profundidade -> nivel
  metodo      -> nivel
  n_exp       -> nivel
  epoca       -> nivel
  definicao   -> nivel
  meta        -> nivel
  modelo      -> nivel
  solo        -> nivel
  tempo        -> nivel
}
") -> dia

svg_xml <- export_svg(dia)
# diagrama
# Salvar como arquivo SVG
writeLines(svg_xml, "meu_diagrama2.svg")


