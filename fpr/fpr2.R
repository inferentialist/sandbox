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

alpha <- 0.1  # was 0.18
half_alpha <- alpha / 2
power <- 0.8
B <- 1 - power
win_rate <- 0.12  # was 0.20

#' Estimate pi = Pr(H0), assume alpha is two sided
estimate_pi <- function(alpha, win_rate, power = 0.8) {
  half_alpha <- alpha / 2
  (power - win_rate) / (power - half_alpha)
}

no_effect_rate <- estimate_pi(alpha, win_rate)
true_effect_rate <- 1 - no_effect_rate
true_effect_rate

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

  las = 1
)

params_x11 <- list(
  width = 7,
  height = 7,
  xpos = 100,
  ypos = 200,
  pointsize = 0.25 * 72
)

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

# the cross-over point from true_effect to no_effect
grid_cross <- ((true_effect_rate * grid_sz) %% grid_blocks) / grid_blocks

# determine the "mass" in each grid cell
grid_fill <- array(0.0, dim = c(grid_blocks, grid_blocks))
grid_fp <- ceiling(true_effect_rate * grid_sz) / grid_sz
avail_te <- (true_effect_rate * power) * grid_sz
avail_ne <- (no_effect_rate * half_alpha) * grid_sz

for (i in seq(0, grid_sz - 1)) {
  c <- 1 + i %% grid_blocks
  r <- 1 + i %/% grid_blocks
  pct <- (i + 1) / grid_sz

  grid_fill[r, c] <- if (pct <= grid_fp) {
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
      - 1.0
    } else if (avail_ne > -1) {
      -(avail_ne + 1)
    } else {
      0.0
    }
  }
}

#' the polygons / lines we want to draw
shapes <- list(
  tep = list(
    x = c(0, grid_cross, grid_cross, 1, 1, 0, 0),
    y = c(yl, yl, yr, yr, 1, 1, yl)
  ),
  nep = list(
    x = c(0, grid_cross, grid_cross, 1, 1, 0, 0),
    y = c(yl, yl, yr, yr, 0, 0, yl)
  ),
  nep_top = list(
    x = c(0, grid_cross, grid_cross, 1),
    y = c(yl, yl, yr, yr)
  ),
  nep_bot = list(
    x = c(1, 1, 0, 0),
    y = c(yr, 0, 0, yl)
  )
)


textbox <- function(keys, labs, cex = 1.0) {

  content <- paste(keys, labs, collapse = "\n")

  vpad <- grconvertY(0.25, "inches", "user")
  h <- strheight(content, cex = cex) + 2 * vpad

  hpad <- grconvertX(0.25, "inches", "user")
  w <- strwidth(content, cex = cex) + 2 * hpad

  rect(0, 0, w, h)

}

keys <- c("\U2611", "\U2612", "\U25A0", "\U25A0")
key_colors <- c("forestgreen", "firebrick1", "#C7E2C7", "#FFCBCB")
labs <- c(
  "a significant real effect",
  "a \"significant\" spurious effect",
  "experiments with a real effect",
  "experiments with no effect"
)



graphics.off()
do.call(x11, params_x11)

p <- withr::with_par(
  params_par,
  {
    do.call(plot, params_plot)

    for (yg in seq(0, 1, length.out = grid_blocks + 1)) {
      lines(c(0, 1), c(yg, yg), col = grid_color)
    }

    for (xg in seq(0, 1, length.out = grid_blocks + 1)) {
      lines(c(xg, xg), c(0, 1), col = grid_color)
    }

    title("Significant Experiments", cex.main = 1.0)

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

    for (ri in seq(1, grid_blocks)) {
      for (ci in seq(1, grid_blocks)) {
        gy <- 1.0 - (ri - 0.5) / grid_blocks
        gx <- (ci - 0.5) / grid_blocks

        gf <- grid_fill[ri, ci]
        if (gf > 0) {
          text(gx, gy, "\U2611", adj =  c(0.5, 0.5), col = "forestgreen", cex = gf)
        } else if (gf < 0) {
          text(gx, gy, "\U2612", adj =  c(0.5, 0.5), col = "firebrick1", cex = -gf)
        }
      }
    }

    vpad <- 0.5 * strheight("", cex = 0.5)
    th <- 1.3 * strheight("", cex = 0.5)
    text(0.95, vpad + 3 * th, "dustin", adj = c(0.5, 0), cex = 0.5)
    text(0.95, vpad + 2 * th, "lennon", adj = c(0.5, 0), cex = 0.5)
    text(0.95, vpad + 1 * th, "@gmail", adj = c(0.5, 0), cex = 0.5)
    text(0.95, vpad + 0 * th, ".com", adj = c(0.5, 0), cex = 0.5)


    cex <- 0.65
    content <- paste(keys, labs)

    line_height <- max(strheight(content, cex = cex))
    line_width <- max(strwidth(content, cex = cex))

    vmar <- grconvertY(c(0, 0.15), from = "inches", to = "user") %>%
      diff()
    vpad <- 1.8 * line_height
    h <- length(content) * vpad + 2 * vmar

    hmar <- grconvertX(c(0, 0.25), from = "inches", to = "user") %>%
      diff()
    hpad <- strwidth("M", cex = cex)
    w <- line_width + 2 * hmar

    xs <- 0.03
    ys <- 0.04
    rect(xs, ys, w + xs, h + ys, col = "white", border = "gray")

    xoff <- xs + hmar + 0.5 * hpad
    yoff <- ys + vmar + 0.5 * line_height
    text(xoff, yoff + 3 * vpad, keys[1], adj = c(0.5, 0), cex = cex, col = key_colors[1])
    text(xoff, yoff + 2 * vpad, keys[2], adj = c(0.5, 0), cex = cex, col = key_colors[2])
    text(xoff, yoff + 1 * vpad, keys[3], adj = c(0.5, 0), cex = cex, col = key_colors[3])
    text(xoff, yoff + 0 * vpad, keys[4], adj = c(0.5, 0), cex = cex, col = key_colors[4])

    xoff2 <- xoff + 1 * hpad
    text(xoff2, yoff + 3 * vpad, labs[1], adj = c(0, 0), cex = cex)
    text(xoff2, yoff + 2 * vpad, labs[2], adj = c(0, 0), cex = cex)
    text(xoff2, yoff + 1 * vpad, labs[3], adj = c(0, 0), cex = cex)
    text(xoff2, yoff + 0 * vpad, labs[4], adj = c(0, 0), cex = cex)


    par()
  }
)

