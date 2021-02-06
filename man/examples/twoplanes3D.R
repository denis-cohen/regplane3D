## ---- Estimation ----
mod <- lm(
  vote ~
    growth +
    centered_approval +
    approval_above_mean +
    growth:approval_above_mean +
    centered_approval:approval_above_mean,
  dat = us
)

## ---- Axis inputs ----
## Cut point
cut_point <- 0
approval_above_mean_vals <- c(0, 1)

## Growth
growth_axis <- pretty_axis_inputs(
  axis_range = range(us$growth),
  base = 2,
  nlines_suggest = 6L,
  multiply = 4
)

## Approval
approval_axis <- pretty_axis_inputs(
  axis_range = c(min(us$centered_approval), cut_point),
  base = 10,
  nlines_suggest = 3L,
  multiply = 4
)

## ---- Prediction ----
pred <-
  array(NA, dim = c(length(growth_axis$seq), length(approval_axis$seq), 2L, 3L))
for (growth in seq_along(growth_axis$seq)) {
  for (centered_approval in seq_along(approval_axis$seq)) {
    for (approval_above_mean in seq_along(approval_above_mean_vals)) {
      pred_tmp <- predict.lm(
        mod,
        newdata = data.frame(
          growth = growth_axis$seq[growth],
          centered_approval = approval_axis$seq[centered_approval] -
            min(approval_axis$seq) *
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
  z = pred[, , 1,],
  x = growth_axis$seq,
  y = approval_axis$seq,
  z2 = pred[, , 2,],
  x2 = growth_axis$seq,
  y2 = approval_axis$seq - min(approval_axis$seq),
  zlim = c(35, 70),
  xlim = growth_axis$range,
  ylim = c(min(approval_axis$seq),-min(approval_axis$seq)),
  zlab = "Predicted Vote Share",
  xlab = "Economic Growth",
  ylab = "Approval Rating \n Above & Below Average",
  cis = TRUE,
  xnlines = growth_axis$nlines,
  ynlines = approval_axis$nlines,
  main = "Incumbent Vote Shares, Economic \n Growth, and Approval Ratings",
  theta = -55,
  phi = 9
)
