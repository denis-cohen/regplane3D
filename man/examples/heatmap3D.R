## --- Data inputs ---
growth_cat_vals <- seq(-4, 6, 2)
approval_cat_vals <-  seq(30, 80, 10)
growth_cat <- cut(us$growth, breaks = growth_cat_vals )
approval_cat <- cut(us$approval, breaks = approval_cat_vals)
joint_frequency <- table(growth_cat, approval_cat)

## ---- Plot ----
heatmap3D(
  z = joint_frequency,
  x = growth_cat_vals,
  y = approval_cat_vals,
  zlab = "Frequency",
  xlab = "Economic Growth",
  ylab = "Approval Rating",
  zlim = range(joint_frequency),
  xlim = range(growth_cat_vals),
  ylim = range(approval_cat_vals),
  main = "Economic Growth and Presidential Approval",
  theta = 225,
  phi = 15
)
