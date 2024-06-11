alpha <- 0.1
power <- 0.8
B <- 1 - power
win_rate <- 0.12

#' Estimate pi = Pr(H0), assume alpha is two sided
estimate_pi <- function(alpha, win_rate, power = 0.8) {
  half_alpha <- alpha / 2
  (power - win_rate) / (power - half_alpha)
}

no_effect_rate <- estimate_pi(alpha, win_rate)
true_effect_rate <- 1 - no_effect_rate

estimate_fpr <- function(alpha, pi, power = 0.8) {
  B <- 1 - power
  half_alpha <- alpha / 2
  (half_alpha * pi) / ((half_alpha * pi) + (1 - B) * (1 - pi))
}

fpr <- estimate_fpr(alpha, no_effect_rate)

params_plot <- list(
  xlim <- c(0, 1),
  ylim <- c(0, 1),
  xlab = "",
  ylab = "",
  type = "n",
  # xaxs = "i",
  # yaxs = "i",
  xaxt = "n",
  yaxt = "n",
  bty = "o",
  asp = 1
)

params_par <- list(
  # bottom, left, top, right
  oma = c(0, 0, 0, 0),
  mar = c(3, 3, 3, 2),

  # axis title, axis labels, axis line
  mgp = c(0, 1, 0),  # c(3,1,0)

  las = 1
)

params_x11 <- list(
  width = 7,
  height = 7,
  xpos = 100,
  ypos = 200,
  pointsize = 0.25 * 72
)

eff_cut <- no_effect_rate
lab_cut <- 1 - 0.5 * alpha

areas <- list(
  t1 = (1 - lab_cut) * eff_cut,
  t2 = (1 - eff_cut) * B,
  g = power * (1 - eff_cut),
  b = eff_cut * lab_cut
)

region_name <- list(
  t1 = "Bg",
  t2 = "Gb",
  g = "Gg",
  b = "Bb"
)

round_digits <- 3
labels <- lapply(areas, round, round_digits)
labels$b <- 1 - labels$g - labels$t1 - labels$t2

template <- stringr::str_glue("%-4s%0.{round_digits}f")
labels <- sapply(
  names(labels),
  \(k) sprintf(template, region_name[[k]], labels[[k]]),
  simplify = FALSE,
  USE.NAMES = TRUE
)

# colors <- list(
#   t1 = adjustcolor("#E41A1C"),
#   t2 = adjustcolor("#FFFF33"),
#   g = adjustcolor("#4DAF4A"),
#   b = adjustcolor("#CBD5E8")
# )

colors <- list(
  t1 = adjustcolor("#E41A1C"),
  t2 = adjustcolor("#FFFF33"),
  g = adjustcolor("#7CA17B"),
  b = adjustcolor("#B5696A")
)


# text_colors <- lapply(colors, colorspace::darken, 0.4)
text_colors <- list(
  t1 = "white",
  t2 = "white",
  g = "white",
  b = "white"
)

graphics.off()
do.call(x11, params_x11)

p <- withr::with_par(
  params_par,
  {
    do.call(plot, params_plot)

    xat <- c(
      (0 + eff_cut) / 2,
      (eff_cut + 1) / 2
    )
    axis(1, at = xat, labels = c("B", "G"))

    yat <- c(
      (0 + lab_cut) / 2,
      (lab_cut + 1) / 2
    )
    axis(2, at = yat, labels = c("b", "g"))

    yat <- c(
      (0 + B) / 2,
      (B + 1) / 2
    )
    axis(4, at = yat, labels = c("b", "g"))


    title("Experiments: [Gg]ood and [Bb]ad", cex.main = 1.0)

    # # Type I errors (bad ideas labeled as good)
    # polygon(
    #   c(0, eff_cut, eff_cut, 0),
    #   c(lab_cut, lab_cut, 1, 1),
    #   border = NA,
    #   col = colors$t1
    # )

    # # Type II errors (good ideas labeled as bad)
    # polygon(
    #   c(eff_cut, 1, 1, eff_cut),
    #   c(0, 0, B, B),
    #   border = NA,
    #   col = colors$t2
    # )

    # # G / g
    # polygon(
    #   c(eff_cut, 1, 1, eff_cut),
    #   c(B, B, 1, 1),
    #   border = NA,
    #   col = colors$g
    # )

    # # B / b
    # polygon(
    #   c(0, eff_cut, eff_cut, 0),
    #   c(0, 0, lab_cut, lab_cut),
    #   border = NA,
    #   col = colors$b
    # )

    # G / g
    polygon(
      c(eff_cut, 1, 1, eff_cut),
      c(0, 0, 1, 1),
      border = NA,
      col = colors$g
    )

    # B / b
    polygon(
      c(0, eff_cut, eff_cut, 0),
      c(0, 0, 1, 1),
      border = NA,
      col = colors$b
    )

    polygon(
      c(0, eff_cut, eff_cut, 1, 1, eff_cut, 0),
      c(lab_cut, lab_cut, B, B, 1, 1, 1),
      # border = NA,
      border = "black",
      lwd = 3,
      col = adjustcolor("white", alpha.f = 0.5)
    )

    polygon(
      c(0, eff_cut, eff_cut, 1, 1, eff_cut, 0),
      c(lab_cut, lab_cut, B, B, 0, 0, 0),
      # border = NA,
      border = "black",
      lwd = 3
    )

    lab_cex <- 0.7
    hpad <- 2 * strwidth(" ", cex = lab_cex)
    vpad <- 0.5 * strheight("", cex = lab_cex)
    vspace <- 1.2 * strheight("", cex = lab_cex)

    text(hpad, vpad + 3 * vspace, labels$g, col = text_colors$g, adj = c(0, 0), cex = lab_cex, family = "mono")
    text(hpad, vpad + 2 * vspace, labels$t1, col = text_colors$t1, adj = c(0, 0), cex = lab_cex, family = "mono")
    text(hpad, vpad + 1 * vspace, labels$t2, col = text_colors$t2, adj = c(0, 0), cex = lab_cex, family = "mono")
    text(hpad, vpad + 0 * vspace, labels$b, col = text_colors$b, adj = c(0, 0), cex = lab_cex, family = "mono")

    text(1, -0.5 * strheight("", cex = 0.5), "dustin.lennon@gmail.com", adj = c(1, 1), cex = 0.5)

    par()
  }
)
