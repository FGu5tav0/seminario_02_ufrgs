library(ggplot2)
library(RColorBrewer)

# set.seed(345)
set.seed(3453)



monochromeR::generate_palette("#D9C4A1", blend_colour = "#6B3E26",
                 n_colours = 8, view_palette = TRUE)

ngroup=50
names=paste("G_",seq(1,ngroup),sep="")
DAT=data.frame()

for(i in seq(1:30)){
  data=data.frame( matrix(0, ngroup , 3))
  data[,1]=i
  data[,2]=sample(names, nrow(data))
  data[,3]=prop.table(sample( c(rep(0,100),c(1:ngroup)) ,nrow(data)))
  DAT=rbind(DAT,data)
}
colnames(DAT)=c("Year","Group","Value")
DAT=DAT[order( DAT$Year, DAT$Group) , ]

coul = brewer.pal(12, "Paired")
coul = brewer.pal(n = 100, "Reds")
coul =  rev(c("#B85C38", "#8A4E24","#D9C4A1", "#4E3620", "#6B3E26"))
coul = colorRampPalette(coul)(ngroup)
coul=coul[sample(c(1:length(coul)) , size=length(coul) ) ]

ggplot(DAT, aes(x=Year, y=Value, fill=Group )) +
  geom_area(alpha=1  ) +
  theme_bw() +
  #scale_fill_brewer(colour="red", breaks=rev(levels(DAT$Group)))+
  scale_fill_manual(values = coul)+
  theme(
    text = element_blank(),
    line = element_blank(),
    title = element_blank(),
    legend.position="none",
    panel.border = element_blank(),
    panel.background = element_blank())

# -------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)

# número de grupos
ngroup <- 200
names <- paste("G_", seq(1, ngroup), sep = "")
DAT <- data.frame()

# simulação de dados
for (i in seq(1:30)) {
  data <- data.frame(matrix(0, ngroup, 3))
  data[, 1] <- i
  data[, 2] <- sample(names, nrow(data))
  data[, 3] <- prop.table(sample(c(rep(0, 100), c(1:ngroup)), nrow(data)))
  DAT <- rbind(DAT, data)
}
colnames(DAT) <- c("Year", "Group", "Value")
DAT <- DAT[order(DAT$Year, DAT$Group), ]

# paleta de cores
coul <- rev(c("#B85C38", "#8A4E24", "#D9C4A1", "#4E3620", "#6B3E26"))
coul <- colorRampPalette(coul)(ngroup)
coul <- coul[sample(seq_along(coul), size = length(coul))]

# gráfico com camadas de baixo ângulo
ggplot(DAT, aes(x = Year, y = Value, fill = Group)) +
  geom_area(alpha = 0.8, position = "identity") +  # sobreposição horizontal
  scale_fill_manual(values = coul) +
  theme_void() +
  theme(legend.position = "none")

ggplot2::ggsave(filename = "teste_3.png")

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



library(aRtsy)

# paleta <- c("#042940","#005C53","#9FC131","#DBF227","#D6D58E")
paleta <- c("#28156e","#291e4e","#f3be0c","#d9241b","#a3192b")
paleta <- rev(c("#B85C38", "#8A4E24","#D9C4A1", "#4E3620", "#6B3E26"))
soil_palette <- c(
  "#b85c38", "#ae5934", "#a55630", "#9b532b", "#915027",
  "#8e542b", "#9f6d45", "#af865f", "#c09f7a", "#d1b894",
  "#cab593", "#ad9778", "#90795d", "#735b42", "#553d27",
  "#533721", "#593922", "#5f3b23", "#653c25", "#6b3e26"
)

angles <- matrix(0, 200, 200)
angles[1:100, ] <- seq(from = 0, to = 2 * pi, length = 100)
angles[101:200, ] <- seq(from = 2 * pi, to = 0, length = 100)
angles <- angles + rnorm(200 * 200, sd = 0.1)

# \donttest{
set.seed(1)
colorPalette()
# Simple example
canvas_stripes(colors = soil_palette, burnin = 100, n = 1000)

canvas_polylines(colors = colorPalette("lava"), iterations = 1000, resolution = 50)

# }



canvas_flow(colors = paleta, lines = 2000,
            background = "white",
            lwd = 1, outline = "square", angles = angles)

set.seed(1)
canvas_splits(colors = paleta,
              # background = "black",
              iterations = 10,
              sd = 0.01,
              lwd = 5,
              alpha = .5)

canvas_phyllotaxis(
  paleta,
  background = "#fafafa",
  iterations = 100000,
  angle = 30,
  size = 1,
  alpha = 1,
  p = 0.1
)

canvas_chladni(
  paleta,
  waves = c(1, 4, 1, 1),
  warp = 4,
  resolution = 1000,
  angles = "worley",
  distances = "cubic",
  flatten = FALSE
)

# ggplot2::ggsave(filename = "teste_3.png")

canvas_cobweb(
  paleta,
  background = "black",
  lines = 300,
  iterations = 100
)


canvas_collatz(
  paleta,
  background = "#fafafa",
  n = 500,
  angle.even = 0.0075,
  angle.odd = 0.0145,
  side = FALSE
)

paleta <- c("#005C53","#DBF227")

canvas_flame(
  paleta,
  # background = "gray99",
  iterations = 1e7,
  variations = c(10, 17),
  # symmetry = -1,
  # blend = TRUE,
  # weighted = FALSE,
  # post = FALSE,
  # final = FALSE,
  # extra = FALSE,
  # display = c("colored", "logdensity"),
  # zoom = 1,
  resolution = 1000,
  # gamma = 5
)


canvas_flow(
  background = "transparent",
  colors = paleta, lines = 390,
  # iterations = 1000,
  lwd = 0.5, polar = F
)

# ggplot2::ggsave(filename = "teste_3.png")


canvas_squares(
  paleta,
  background = "transparent",
  cuts = 300,
  resolution = 200,
  noise = F
)

# ggplot2::ggsave(filename = "fundo_1.png")
