## ---- Estimation ----
mod <-
  lm(vote ~
       growth +
       centered_approval +
       approval_above_mean +
       growth:approval_above_mean +
       centered_approval:approval_above_mean,
     dat = us)

## ---- Prediction ----
growth_range <- c(-4, 6)
approval_range <- c(-20, 30)
approval_above_mean_vals <- c(0, 1)
growth_seq <-
  seq(growth_range[1], growth_range[2], length.out = 21L)
approval_seq <-
  seq(approval_range[1], 0, length.out = 21L)

pred <- array(NA, dim = c(21L, 21L, 2L, 3L))
for (growth in seq_along(growth_seq)) {
  for (centered_approval in seq_along(approval_seq)) {
    for (approval_above_mean in seq_along(approval_above_mean_vals)) {
      pred_tmp <- predict.lm(
        mod,
        newdata = data.frame(
          growth = growth_seq[growth],
          centered_approval = approval_seq[centered_approval] -
            min(approval_seq) *
            approval_above_mean_vals[approval_above_mean],
          approval_above_mean = approval_above_mean_vals[approval_above_mean]
        ),
        se.fit = TRUE
      )
      pred[growth, centered_approval, approval_above_mean,] <-
        c(
          pred_tmp$fit,
          pred_tmp$fit + qnorm(.025) * pred_tmp$se.fit,
          pred_tmp$fit + qnorm(.975) * pred_tmp$se.fit
        )
    }
  }
}

## ---- Plot ----
twoplanes3D(
  z = pred[, , 1, ],
  x = growth_seq,
  y = approval_seq,
  z2 = pred[, , 2, ],
  x2 = growth_seq ,
  y2 = approval_seq - min(approval_seq),
  zlab = "Predicted Vote Share",
  xlab = "Economic Growth",
  ylab = "Approval Rating \n Above & Below Average",
  zlim = c(30, 75),
  xlim = growth_range,
  ylim = c(min(approval_seq), -min(approval_seq)),
  nlines = 7,
  main = "Incumbent Vote Shares, Economic \n Growth, and Approval Ratings",
  theta = -55,
  phi = 9
)
