library(magrittr)
library(rlang)

rm.all <- function(exclusions = c()) { # nolint
  exclusions <- c("workflow", exclusions)

  all_names <- global_env() %>%
    names()

  rm(
    list = setdiff(all_names, exclusions),
    envir = global_env()
  )
}

rm.all()

#' statistics -----------------------------------------------------------------


alpha <- 0.1
half_alpha <- alpha / 2
power <- 0.8
B <- 1 - power
win_rate <- 0.12

#' estimate pi = Pr(H0), assume alpha is two sided
estimate_pi <- function(alpha, win_rate, power = 0.8) {
  half_alpha <- alpha / 2
  (power - win_rate) / (power - half_alpha)
}

no_effect_rate <- estimate_pi(alpha, win_rate)
true_effect_rate <- 1 - no_effect_rate
true_effect_rate

#' estimate false positive rate
estimate_fpr <- function(alpha, pi, power = 0.8) {
  B <- 1 - power
  half_alpha <- alpha / 2
  (half_alpha * pi) / ((half_alpha * pi) + (1 - B) * (1 - pi))
}
fpr <- estimate_fpr(alpha, no_effect_rate)

#' plotting related parameters ------------------------------------------------
params_plot <- list(
  xlim <- c(0, 1),
  ylim <- c(0, 1),
  xlab = "",
  ylab = "",
  type = "n",
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  asp = 1
)

params_par <- list(
  # bottom, left, top, right
  oma = c(0, 0, 0, 0),
  mar = c(1, 1, 2, 1),

  # axis title, axis labels, axis line
  mgp = c(0, 1, 0),  # c(3,1,0)

  las = 1,
  bg = "white"
)

params_x11 <- list(
  width = 7,
  height = 7,
  xpos = 100,
  ypos = 200,
  pointsize = 0.25 * 72
)

#' visualization components ---------------------------------------------------

grid_blocks <- 10
grid_sz <- grid_blocks * grid_blocks
grid_color <- "steelblue3"
grid_delta <- 1 / grid_blocks
grid_ter <- floor(true_effect_rate / grid_delta)
grid_ner <- floor(no_effect_rate / grid_delta)


# determine the left and right y-values
i <- 0
while (i + grid_ner + grid_ter < grid_blocks) {
  i <- i + 1
}

if (i > 1) {
  abort("shift assertion failure")
}

yl <- grid_ner * grid_delta
yr <- (grid_ner + i) * grid_delta

# determine the cross-over point from true_effect to no_effect
grid_cross <- ((true_effect_rate * grid_sz) %% grid_blocks) / grid_blocks

# determine the "mass" in each grid cell; distribute mass until we run out
grid_fill <- array(0.0, dim = c(grid_blocks, grid_blocks))
grid_label <- array(0L, dim = c(grid_blocks, grid_blocks))
grid_fp <- ceiling(true_effect_rate * grid_sz) / grid_sz
avail_te <- (true_effect_rate * power) * grid_sz
avail_ne <- (no_effect_rate * half_alpha) * grid_sz

for (i in seq(0, grid_sz - 1)) {
  ci <- 1 + i %% grid_blocks
  ri <- 1 + i %/% grid_blocks
  pct <- (i + 1) / grid_sz

  grid_fill[ri, ci] <- if (pct <= grid_fp) {
    avail_te <- avail_te - 1
    if (avail_te > 0) {
      1.0
    } else if (avail_te > -1) {
      avail_te + 1
    } else {
      0.0
    }
  } else {
    avail_ne <- avail_ne - 1
    if (avail_ne > 0) {
      1.0
    } else if (avail_ne > -1) {
      avail_ne + 1
    } else {
      0.0
    }
  }

  grid_label[ri, ci] <- if (pct < grid_fp) 1L else if (pct > grid_fp) -1L else 0L
}

# the polygons / lines we want to draw
shapes <- list(
  tep = list(
    x = c(0, grid_cross, grid_cross, 1, 1, 0, 0),
    y = c(yl, yl, yr, yr, 1, 1, yl)
  ),
  nep = list(
    x = c(0, grid_cross, grid_cross, 1, 1, 0, 0),
    y = c(yl, yl, yr, yr, 0, 0, yl)
  )
)

# glyphs
glyph_full <- "\U25A0"
glyph_empty <- "\U25A1"
glyph_partial <- "\U25A7"

# textbox elements ------------------------------------------------------------

keys <- c(glyph_full, glyph_full, "", glyph_full, glyph_full, glyph_empty)

key_colors <- c("#C7E2C7", "#FFCBCB", "white", "forestgreen", "firebrick1", "black")

labs <- c(
  "experiments with a real effect",
  "experiments with no effect",
  "",
  "a significant real effect",
  "a \"significant\" spurious effect",
  "an unadopted treatment"
)

# the percent text labels
round_digits <- 3
pcts <- list(
  true_effect = 100 * round(true_effect_rate, round_digits),
  true_pos = 100 * round(true_effect_rate * power, round_digits)
)
pcts$spurious_effect <- 100 - pcts$true_pos
pcts$false_neg <- 100 * round(half_alpha * no_effect_rate, round_digits)
pcts$unadopted <- 100 - pcts$true_pos - pcts$false_neg

template <- stringr::str_glue("%{round_digits+2}.{round_digits-2}f%%")
labels <- sapply(
  pcts,
  \(p) sprintf(template, p),
  simplify = FALSE,
  USE.NAMES = TRUE
)

pct_lab <- c(
  labels$true_effect,
  labels$spurious_effect,
  "",
  labels$true_pos,
  labels$false_neg,
  labels$unadopted
)
pct_lab

# design credits --------------------------------------------------------------

options(design_credit = TRUE)
is_design_cell <- function(ri, ci) {
  ri == grid_blocks && ci == grid_blocks && getOption("design_credit", default = FALSE)
}


# create the figure -----------------------------------------------------------
graphics.off()
do.call(x11, params_x11)

gpar <- withr::with_par(
  params_par,
  {
    do.call(plot, params_plot)

    # horizonal grid lines
    for (yg in seq(0, 1, length.out = grid_blocks + 1)) {
      lines(c(0, 1), c(yg, yg), col = grid_color)
    }

    # vertical grid lines
    for (xg in seq(0, 1, length.out = grid_blocks + 1)) {
      lines(c(xg, xg), c(0, 1), col = grid_color)
    }

    title("Significant Experiments", cex.main = 1.0)

    # true effect / no effect regions
    polygon(
      shapes$tep,
      col = adjustcolor("forestgreen", alpha.f = 0.25),
      border = NA
    )

    polygon(
      shapes$nep,
      col = adjustcolor("firebrick1", alpha.f = 0.25),
      border = NA
    )

    # loop over grid and add colored glyph to each cell
    for (ri in seq(1, grid_blocks)) {
      for (ci in seq(1, grid_blocks)) {
        gy <- 1.0 - (ri - 0.5) / grid_blocks
        gx <- (ci - 0.5) / grid_blocks

        gf <- grid_fill[ri, ci]
        gl <- grid_label[ri, ci]

        col <- if (gl == 1L) {
          "forestgreen"
        } else if (gl == -1L) {
          "firebrick1"
        } else {
          "gray"
        }

        if (gf == 1L) {
          text(gx, gy, glyph_full, adj = c(0.5, 0.5), col = col, family = "Noto Mono")  # check \U2611
        } else if (gf > 0L) {
          text(gx, gy, glyph_partial, adj = c(0.5, 0.5), col = col, family = "Noto Mono")
        } else if (!is_design_cell(ri, ci)) {
          text(gx, gy, glyph_empty, adj = c(0.5, 0.5), col = col, family = "Noto Mono")
        }
      }
    }

    # custom textbox
    cex <- 0.5
    msw <- max(nchar(labs))
    template <- stringr::str_glue("%-5s %-{msw}s %s")
    content <- sprintf(template, keys, labs, pct_lab)
    content

    line_height <- max(strheight(content, cex = cex, family = "Noto Mono"))
    line_width <- max(strwidth(content, cex = cex, family = "Noto Mono"))

    vmar <- grconvertY(c(0, 0.15), from = "inches", to = "user") %>%
      diff()
    vpad <- 1.8 * line_height
    h <- length(content) * vpad + 2 * vmar

    hmar <- grconvertX(c(0, 0.15), from = "inches", to = "user") %>%
      diff()
    hpad <- strwidth("M", cex = cex, family = "Noto Mono")
    w <- line_width + 2 * hmar

    xs <- 0.03
    ys <- 0.04
    rect(xs, ys, w + xs, h + ys, col = adjustcolor("white", alpha.f = 0.9), border = "gray")

    xoff <- xs + hmar + 0.5 * hpad
    xoff2 <- xoff + 2 * hpad
    xoff3 <- xoff2 + 33 * hpad
    yoff <- ys + vmar + 0.5 * line_height
    nl <- length(labs)
    for (j in seq_along(labs)) {
      k <- nl - j
      text(xoff,  yoff + k * vpad, keys[j], adj = c(0, 0), cex = cex, col = key_colors[j], family = "Noto Mono")
      text(xoff2, yoff + k * vpad, labs[j], adj = c(0, 0), cex = cex, family = "Noto Mono")
      text(xoff3, yoff + k * vpad, pct_lab[j], adj = c(0, 0), cex = cex, family = "Noto Mono")
    }

    # design credit
    if (getOption("design_credit", default = FALSE)) {
      vpad <- 0.5 * strheight("", cex = 0.5)
      th <- 1.3 * strheight("", cex = 0.5)
      text(0.95, vpad + 3 * th, "dustin", adj = c(0.5, 0), cex = 0.5)
      text(0.95, vpad + 2 * th, "lennon", adj = c(0.5, 0), cex = 0.5)
      text(0.95, vpad + 1 * th, "@gmail", adj = c(0.5, 0), cex = 0.5)
      text(0.95, vpad + 0 * th, ".com", adj = c(0.5, 0), cex = 0.5)
    }

    invisible(par())
  }
)

dev_id <- dev.copy(png, "fpr/fig2.png")
dev.off(dev_id)
