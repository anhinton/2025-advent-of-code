options(scipen = 9)

test = ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............." |>
  strsplit("\n") |>
  unlist()

input = readLines("input/day07_input.txt")

## Part 1 ----

count_splits = function(x) {
  diagram_matrix = x |>
    lapply(
      FUN = strsplit
      , split = ""
    ) |>
    unlist() |>
    matrix(
      nrow = length(x)
      , byrow = TRUE
    )
  split = 0
  for(i in 2:nrow(diagram_matrix)) {
    for (j in 1:ncol(diagram_matrix)) {
      if (diagram_matrix[i - 1, j] %in% c("S", "|")) {
        if (diagram_matrix[i, j] == ".") {
          diagram_matrix[i, j] = "|"
        } else if (diagram_matrix[i, j] == "^" ) {
          this_row = c(j - 1, j + 1)
          this_row = this_row[this_row >= 0 & this_row <= ncol(diagram_matrix)]
          diagram_matrix[i, this_row] = "|"
          split = split + 1
        }
      }
    }
  }
  split
}

count_splits(test) == 21

count_splits(input)

## Part 2 ----

x = test

count_timelines = function(x) {
  diagram_matrix = x |>
    lapply(
      FUN = strsplit
      , split = ""
    ) |>
    unlist() |>
    matrix(
      nrow = length(x)
      , byrow = TRUE
    )
  paths_matrix = matrix(
    data = 0
    , nrow = nrow(diagram_matrix)
    , ncol = ncol(diagram_matrix)
  )
  split = 0
  for(i in 2:nrow(diagram_matrix)) {
    for (j in 1:ncol(diagram_matrix)) {
      if (diagram_matrix[i - 1, j] == "S") {
        diagram_matrix[i, j] = "|"
        paths_matrix[i, j] = 1
      } else if (diagram_matrix[i - 1, j]  == "|") {
        if (diagram_matrix[i, j] %in% c(".", "|")) {
          diagram_matrix[i, j] = "|"
          paths_matrix[i, j] = paths_matrix[i, j] + paths_matrix[i - 1, j]
        } else if (diagram_matrix[i, j] == "^") {
          this_row = c(j - 1, j + 1)
          this_row = this_row[this_row >= 0 & this_row <= ncol(diagram_matrix)]
          diagram_matrix[i, this_row] = "|"
          paths_matrix[i, this_row] = paths_matrix[i, this_row] + paths_matrix[i - 1, j]
          split = split + 1
        }
      }
    }
  }
  sum(paths_matrix[nrow(paths_matrix), ])
}

count_timelines(test) == 40

count_timelines(input)
