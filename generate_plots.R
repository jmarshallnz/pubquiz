source("score_plot.R")

conf <- seq(0,1,by=0.05)
round <- 1:2

for (r in round) {
  for (w in conf) {
    file <- sprintf("round%d_%03.0f.png", r, w*100)
    png(file.path("plots", file), bg="transparent", width=500, height=400)
    score_plot(w,r)
    dev.off()
  }
}
