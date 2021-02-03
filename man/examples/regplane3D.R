## ---- Estimation ----
mod <- lm(vote ~
            growth +
            approval,
          dat = us)

## ---- Prediction ----
growth_range <- c(-4, 6)
approval_range <- c(30, 80)
growth_seq <-
  seq(growth_range[1], growth_range[2], length.out = 21L)
approval_seq <-
  seq(approval_range[1], approval_range[2], length.out = 21L)

pred <- array(NA, dim = c(21L, 21L, 3L))
for (growth in seq_along(growth_seq)) {
  for (approval in seq_along(approval_seq)) {
    pred_tmp <- predict.lm(
      mod,
      newdata = data.frame(growth = growth_seq[growth],
                           approval = approval_seq[approval]),
      se.fit = TRUE
    )
    pred[growth, approval,] <- c(
      pred_tmp$fit,
      pred_tmp$fit + qnorm(.025) * pred_tmp$se.fit,
      pred_tmp$fit + qnorm(.975) * pred_tmp$se.fit
    )
  }
}

## ---- Plot ----
regplane3D(
  pred,
  growth_seq,
  approval_seq,
  zlab = "Predicted Vote Share",
  xlab = "Economic Growth",
  ylab = "Approval Rating",
  zlim = c(35, 70),
  xlim = growth_range,
  ylim = approval_range,
  cis = TRUE,
  nlines = 7,
  main = "Incumbent Vote Shares, Economic \n Growth, and Approval Ratings",
  theta = -45,
  phi = 9
)
