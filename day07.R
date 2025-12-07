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
  # paths = 0
  for(i in 2:nrow(diagram_matrix)) {
    for (j in 1:ncol(diagram_matrix)) {
      if (diagram_matrix[i - 1, j] %in% c("S", "|")) {
        if (diagram_matrix[i, j] == ".") {
          diagram_matrix[i, j] = "|"
        } else if (diagram_matrix[i, j] == "^" ) {
          this_row = c(j - 1, j + 1)
          diagram_matrix[i, sample(this_row, size = 1)] = "|"
          # paths = paths + 2
        }
      }
    }
    # if (i %% 2 == 0) {
    #   cat(
    #     apply(diagram_matrix, MAR = 1, FUN = paste, collapse = "")
    #     , sep = "\n"
    #   )
    #   print(paste("i:", i, "; paths:", paths))
    # }
  }
  # paths
  cat(
    apply(diagram_matrix, MAR = 1, FUN = paste, collapse = "")
    , sep = "\n"
  )
}

count_timelines(test)
