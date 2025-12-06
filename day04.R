test = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." |>
  strsplit("\n") |>
  unlist()

input = readLines("input/day04_input.txt")

count_rolls = function(x) {
  x_vector = sapply(
    X = x
    , FUN = strsplit
    , split = ""
    , USE.NAMES = FALSE
  ) |>
    unlist()
  m = matrix(data = x_vector, nrow = length(x), byrow = TRUE)
  count = matrix(numeric(), nrow = nrow(m), ncol = ncol(m))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (m[i, j] == "@") {
        rows = c(i - 1, i, i + 1)
        rows = rows[rows > 0 & rows <= nrow(m)]
        cols = c(j - 1, j, j + 1)
        cols = cols[cols > 0 & cols <= nrow(m)]
        count[i, j] = sum(m[rows, cols] == "@") - 1
      } else {
        NA
      }
    }
  }
  sum(count < 4, na.rm = TRUE)
}

count_rolls(test) == 13

count_rolls(input)

rolls_to_remove = function(m) {
  count = matrix(numeric(), nrow = nrow(m), ncol = ncol(m))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (m[i, j] == "@") {
        rows = c(i - 1, i, i + 1)
        rows = rows[rows > 0 & rows <= nrow(m)]
        cols = c(j - 1, j, j + 1)
        cols = cols[cols > 0 & cols <= nrow(m)]
        count[i, j] = sum(m[rows, cols] == "@") - 1
      } else {
        count[i, j] = 99
      }
    }
  }
  count < 4
}

total_rolls_to_remove = function(x) {
  x_vector = sapply(
    X = x
    , FUN = strsplit
    , split = ""
    , USE.NAMES = FALSE
  ) |>
    unlist()
  m = matrix(data = x_vector, nrow = length(x), byrow = TRUE)
  
  removed = 0
  while(TRUE) {
    to_remove = rolls_to_remove(m)
    if (sum(to_remove) == 0) {
      break
    }
    removed = removed + sum(to_remove)
    m[to_remove] = "x"
  }
  removed
}

total_rolls_to_remove(test) == 43

total_rolls_to_remove(input)
