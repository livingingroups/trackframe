# Expose internal function
scale_arrows <- trackframe:::scale_arrows

# Setup plot so that grconvert give something
png(withr::local_tempfile())
plot(0:1, 0:1)

xeps <- diff(grconvertX(c(0, 1e-3), from = "inches", to = "user"))
yeps <- diff(grconvertY(c(0, 1e-3), from = "inches", to = "user"))

expect_equal(
  scale_arrows(
    list(xmin = 0, ymin = 0, xmax = 0, ymax = 0),
    .5
  ),
  list(xmin = 0, ymin = 0, xmax = 0, ymax = 0)
)

expect_equal(
  scale_arrows(
    list(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
    0
  ),
  list(xmin = 0, ymin = 0, xmax = xeps, ymax = yeps)
)

expect_equal(
  scale_arrows(
    list(xmin = 0, ymin = 0, xmax = xeps * .1, ymax = yeps * .1),
    0
  ),
  list(xmin = 0, ymin = 0, xmax = xeps, ymax = yeps)
)

expect_equal(
  scale_arrows(
    list(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
    1
  ),
  list(xmin = 0, ymin = 0, xmax = 1 + xeps, ymax = 1 + yeps)
)

expect_equal(
  scale_arrows(
    list(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
    .5
  ),
  list(xmin = 0, ymin = 0, xmax = .5 + xeps, ymax = .5 + yeps)
)

expect_equal(
  scale_arrows(
    list(xmin = 0, ymin = 0, xmax = -1, ymax = 1),
    .5
  ),
  list(xmin = 0, ymin = 0, xmax = -.5 - xeps, ymax = .5 + yeps)
)

expect_equal(
  scale_arrows(
    list(xmin = .5, ymin = -.5, xmax = -1, ymax = 1),
    .5
  ),
  list(xmin = .5, ymin = -.5, xmax = -.25 - xeps, ymax = .25 + yeps)
)
