# tests/testthat/test-the_minimalist.R

test_that("basic feature discrimination works", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, 5)
  )
  
  set.seed(123)
  result <- the_minimalist(data)
  
  expect_type(result, "list")
  expect_named(result, c("name", "cues"))
  expect_equal(result$name, "A")
  expect_true("feature1" %in% result$cues)
})

test_that("recognition heuristic takes precedence", {
  data <- data.frame(
    Name = c("A", "B"),
    recognized = c(1, 0),
    # B has higher feature but A is recognized
    feature1 = c(5, 10)
  )
  
  result <- the_minimalist(data)
  
  expect_equal(result$name, "A")
  expect_equal(result$cues, "recognized")
})

test_that("recognition heuristic skipped when both recognized", {
  data <- data.frame(
    Name = c("A", "B"),
    recognized = c(1, 1),
    feature1 = c(10, 5)
  )
  
  set.seed(123)
  result <- the_minimalist(data)
  
  expect_equal(result$name, "A")
  expect_true("feature1" %in% result$cues)
})

test_that("recognition heuristic skipped when neither recognized", {
  data <- data.frame(
    Name = c("A", "B"),
    recognized = c(0, 0),
    feature1 = c(10, 5)
  )
  
  set.seed(123)
  result <- the_minimalist(data)
  
  expect_equal(result$name, "A")
  expect_true("feature1" %in% result$cues)
})

test_that("NA handling with na_heuristic = TRUE", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, NA)
  )
  
  result <- the_minimalist(data, na_heuristic = TRUE)
  
  expect_equal(result$name, "A")
  expect_equal(result$cues, "feature1")
})

test_that("NA handling with na_heuristic = FALSE skips feature", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, NA),
    feature2 = c(5, 10)
  )
  
  set.seed(456)
  result <- the_minimalist(data, na_heuristic = FALSE)
  
  # Should skip feature1 and use feature2
  expect_equal(result$name, "B")
  expect_true("feature2" %in% result$cues)
})

test_that("tied features continue to next feature", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, 10),
    feature2 = c(5, 15)
  )
  
  set.seed(789)
  result <- the_minimalist(data)
  
  # Only assert that feature2 was eventually used
  expect_true("feature2" %in% result$cues)
  # If feature1 was checked first, both should be present
  if ("feature1" %in% result$cues) {
    expect_length(result$cues, 2)
  }
})

test_that("all features tied results in random choice", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(10, 10),
    feature2 = c(5, 5)
  )
  
  set.seed(111)
  results <- replicate(20, the_minimalist(data), simplify = FALSE)
  
  names <- sapply(results, function(x) x$name)
  cues <- lapply(results, function(x) x$cues)
  
  # Both A and B should appear at least once
  expect_true("A" %in% names)
  expect_true("B" %in% names)
  
  # All runs should check both features
  expect_true(all(sapply(cues, length) == 2))
})

test_that("custom id column works", {
  data <- data.frame(
    Item = c("X", "Y"),
    feature1 = c(10, 5)
  )
  
  set.seed(222)
  result <- the_minimalist(data, id = "Item")
  
  expect_equal(result$name, "X")
})

test_that("multiple features randomly sampled", {
  data <- data.frame(
    Name = c("A", "B"),
    f1 = c(1, 1),
    f2 = c(2, 2),
    f3 = c(5, 10)
  )
  
  # Run multiple times to ensure randomness works
  set.seed(333)
  results <- replicate(10, {
    the_minimalist(data)$cues
  }, simplify = FALSE)
  
  # Should eventually check all features before finding f3
  expect_true(any(sapply(results, function(x) length(x) >= 2)))
})

test_that("both items NA with na_heuristic = FALSE skips feature", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(NA, NA),
    feature2 = c(10, 5)
  )
  
  set.seed(444)
  result <- the_minimalist(data, na_heuristic = FALSE)
  
  expect_equal(result$name, "A")
  expect_true("feature2" %in% result$cues)
})

test_that("all features NA results in random choice", {
  data <- data.frame(
    Name = c("A", "B"),
    feature1 = c(NA, NA),
    feature2 = c(NA, NA)
  )
  
  set.seed(555)
  results <- replicate(20, the_minimalist(data, na_heuristic = FALSE), 
                      simplify = FALSE)
  
  names <- sapply(results, function(x) x$name)
  cues <- lapply(results, function(x) x$cues)
  
  # Both A and B should appear at least once
  expect_true("A" %in% names)
  expect_true("B" %in% names)
  
  # All runs should check both features
  expect_true(all(sapply(cues, length) == 2))
})