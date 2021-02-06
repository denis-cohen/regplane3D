## Find range of variable
growth_range <- range(us$growth)
growth_range

## Determine axis inputs
growth_axis <- pretty_axis_inputs(
  axis_range = growth_range,
  base = 2,
  nlines_suggest = 6L,
  multiply = 4
)
growth_axis
