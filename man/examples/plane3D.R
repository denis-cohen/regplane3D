## ---- Estimation ----
mod <- lm(vote ~
            growth +
            approval,
          dat = us)

## ---- Axis inputs ----
## Growth
growth_axis <- pretty_axis_inputs(
  axis_range = range(us$growth),
  base = 2,
  nlines_suggest = 6L,
  multiply = 4
)

## Approval
approval_axis <- pretty_axis_inputs(
  axis_range = range(us$approval),
  base = 10,
  nlines_suggest = 6L,
  multiply = 4
)


## ---- Prediction ----
pred <-
  array(NA, dim = c(length(growth_axis$seq), length(approval_axis$seq), 3L))
for (growth in seq_along(growth_axis$seq)) {
  for (approval in seq_along(approval_axis$seq)) {
    pred_tmp <- predict.lm(
      mod,
      newdata = data.frame(growth = growth_axis$seq[growth],
                           approval = approval_axis$seq[approval]),
      se.fit = TRUE
    )
    pred[growth, approval, ] <- c(
      pred_tmp$fit,
      pred_tmp$fit + qnorm(.025) * pred_tmp$se.fit,
      pred_tmp$fit + qnorm(.975) * pred_tmp$se.fit
    )
  }
}

## ---- Plot ----
plane3D(
  z = pred,
  x = growth_axis$seq,
  y = approval_axis$seq,
  zlab = "Predicted Vote Share",
  xlab = "Economic Growth",
  ylab = "Approval Rating",
  zlim = c(35, 70),
  xlim = growth_axis$range,
  ylim = approval_axis$range,
  cis = TRUE,
  xnlines = growth_axis$nlines,
  ynlines = approval_axis$nlines,
  main = "Incumbent Vote Shares, Economic \n Growth, and Approval Ratings",
  theta = -45,
  phi = 9
)
