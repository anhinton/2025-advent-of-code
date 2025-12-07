options(scipen = 9)

test = "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  " |> 
  strsplit(split = "\n") |>
  unlist()

input = readLines("input/day06_input.txt")

x = input
sum_sheet = function(x) {
  x_list = lapply(
    X = x
    , FUN = function(x) {
      x = gsub("^[[:space:]]*", "", x)
      x = gsub("[[:space:]]*$", "", x)
      unlist(strsplit(x, split = "[[:space:]]+"))
    }
  )
  numbers = matrix(
    data = as.numeric(unlist(x_list[-length(x_list)]))
    , nrow = length(x_list) - 1
    , byrow = TRUE
  )
  operations = x_list[[length(x_list)]]
  
  results = lapply(
    X = 1:ncol(numbers)
    , FUN = function(i) {
      if (operations[i] == "*") {
        prod(numbers[, i])
      } else if (operations[i] == "+") {
        sum(numbers[, i])
      }
    }
  )
  sum(unlist(results))
}

sum_sheet(test) == 4277556

sum_sheet(input)

x = test
sum_sheet_vertical = function(x) {
  numbers = x |> 
    lapply(
      , FUN = strsplit
      , split = ""
    ) |>
    unlist() |>
    matrix(nrow = length(x), byrow = TRUE) |>
    t() |>
    apply(
      MAR = 1
      , FUN = function(x) {
        as.numeric(gsub("[[:punct:]]", "", paste(x, collapse = "")))
      }
    )
  start = c(1, which(is.na(numbers)) + 1)
  end = c(which(is.na(numbers)) -1, length(numbers))
  columns = lapply(
    X = seq_along(start)
    , FUN = function(i) {
      numbers[start[i]:end[i]]
    }
  )
  operations = gsub("[[:space:]]", "", x[length(x)]) |>
    strsplit("") |>
    unlist()
  results = lapply(
    X = seq_along(columns)
    , FUN = function(i) {
      if (operations[i] == "*") {
        prod(columns[[i]])
      } else if (operations[i] == "+") {
        sum(columns[[i]])
      }
    }
  )
  sum(unlist(results))
}

sum_sheet_vertical(test) == 3263827

sum_sheet_vertical(input)
