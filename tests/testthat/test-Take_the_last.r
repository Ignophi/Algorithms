# tests/testthat/test-take_the_last.R

test_that("basic feature discrimination works", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, 5),
    feature2 = c(3, 8)
  )
  
  result <- take_the_last(data)
  
  expect_type(result, "list")
  expect_named(result, c("name", "cues", "f_order"))
  expect_equal(result$name, "A")
  expect_equal(result$cues, "feature1")
  # feature1 discriminated, should be moved to front
  expect_equal(result$f_order, c("feature1", "feature2"))
})

test_that("f_order initialization from data when NULL", {
  data <- data.frame(
    Name = c("A", "B"),
    feature2 = c(3, 8),
    feature1 = c(10, 5)
  )
  
  result <- take_the_last(data)
  
  # Should initialize f_order from column order (excluding Name)
  expect_equal(result$f_order, c("feature2", "feature1"))
})

test_that("provided f_order is respected", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(5, 5),
    feature2 = c(3, 8)
  )
  
  # Provide custom order
  result <- take_the_last(data, f_order = c("feature2", "feature1"))
  
  expect_equal(result$name, "B")
  expect_equal(result$cues, "feature2")
  # feature2 discriminated, stays at front
  expect_equal(result$f_order, c("feature2", "feature1"))
})

test_that("discriminating feature moves to front", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(5, 5),
    feature2 = c(3, 8)
  )
  
  # feature1 is first but ties, so feature2 discriminates
  result <- take_the_last(data, f_order = c("feature1", "feature2"))
  
  expect_equal(result$name, "B")
  expect_equal(result$cues, c("feature1", "feature2"))
  # feature2 moved to front
  expect_equal(result$f_order, c("feature2", "feature1"))
})

test_that("learning effect across multiple calls", {
  data1 <- data.frame(
    Name = c("A", "B"),
    feature1 = c(5, 5),
    feature2 = c(10, 3)
  )
  
  # First call
  result1 <- take_the_last(data1)
  expect_equal(result1$cues, c("feature1", "feature2"))
  expect_equal(result1$f_order, c("feature2", "feature1"))
  
  # Second call with updated f_order
  data2 <- data.frame(
    Name = c("C", "D"),
    feature1 = c(7, 2),
    feature2 = c(4, 4)
  )
  
  result2 <- take_the_last(data2, f_order = result1$f_order)
  # feature2 is checked first but ties, then feature1 discriminates
  expect_equal(result2$cues, c("feature2", "feature1"))
  expect_equal(result2$name, "C")
  expect_equal(result2$f_order, c("feature1", "feature2"))
})

test_that("f_order filters out missing features", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, 5)
  )
  
  # Provide f_order with features not in data
  result <- take_the_last(data, f_order = c("feature3", "feature1", "feature2"))
  
  expect_equal(result$name, "A")
  # Only feature1 should remain
  expect_equal(result$f_order, "feature1")
})

test_that("recognition heuristic takes precedence", {
  data <- data.frame(
    Name = c("A", "B"),
    recognized = c(1, 0),
    feature1 = c(5, 10)
  )
  
  result <- take_the_last(data)
  
  expect_equal(result$name, "A")
  expect_equal(result$cues, "recognized")
  # f_order unchanged
  expect_equal(result$f_order, "feature1")
})

test_that("NA handling with na_heuristic = TRUE", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, NA)
  )
  
  result <- take_the_last(data, na_heuristic = TRUE)
  
  expect_equal(result$name, "A")
  expect_equal(result$cues, "feature1")
  expect_equal(result$f_order, "feature1")
})

test_that("NA handling with na_heuristic = FALSE skips feature", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, NA),
    feature2 = c(5, 10)
  )
  
  result <- take_the_last(data, na_heuristic = FALSE)
  
  expect_equal(result$name, "B")
  expect_true("feature2" %in% result$cues)
  # feature2 moves to front
  expect_equal(result$f_order, c("feature2", "feature1"))
})

test_that("all features tied results in random choice", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, 10),
    feature2 = c(5, 5)
  )
  
  set.seed(111)
  results <- replicate(20, take_the_last(data), simplify = FALSE)
  
  names <- sapply(results, function(x) x$name)
  cues <- lapply(results, function(x) x$cues)
  f_orders <- lapply(results, function(x) x$f_order)
  
  # Both A and B should appear at least once
  expect_true("A" %in% names)
  expect_true("B" %in% names)
  
  # All runs should check both features
  expect_true(all(sapply(cues, length) == 2))
  
  # f_order should remain unchanged (no discrimination)
  expect_true(all(sapply(f_orders, function(x) identical(x, c("feature1", "feature2")))))
})

test_that("custom id column works", {
  data <- data.frame(
    Item = c("X", "Y"),
    feature1 = c(10, 5)
  )
  
  result <- take_the_last(data, id = "Item")
  
  expect_equal(result$name, "X")
})

test_that("all features NA results in random choice", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(NA, NA),
    feature2 = c(NA, NA)
  )
  
  set.seed(555)
  results <- replicate(20, take_the_last(data, na_heuristic = FALSE), 
                      simplify = FALSE)
  
  names <- sapply(results, function(x) x$name)
  cues <- lapply(results, function(x) x$cues)
  f_orders <- lapply(results, function(x) x$f_order)
  
  # Both A and B should appear at least once
  expect_true("A" %in% names)
  expect_true("B" %in% names)
  
  # All runs should check both features
  expect_true(all(sapply(cues, length) == 2))
  
  # f_order unchanged
  expect_true(all(sapply(f_orders, function(x) identical(x, c("feature1", "feature2")))))
})