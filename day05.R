options(scipen = 9)

test = "3-5
10-14
16-20
12-18

1
5
8
11
17
32" |>
  strsplit(split = "\n") |>
  unlist()

input = readLines("input/day05_input.txt")

count_fresh_ids = function(x) {
  blank = which(x == "")
  ids = as.numeric(x[(blank + 1):length(x)])
  ranges = lapply(
    X = x[1:(blank - 1)]
    , FUN = function(x) {
      from = regexpr("^[[:digit:]]+-", text = x)
      from = regmatches(x = x, m = from)
      from = gsub("-$", "", from)
      to = regexpr("-[[:digit:]]+$", text = x)
      to = regmatches(x = x, m = to)
      to = gsub("^-", "", to)
      as.numeric(c(from, to))
    }
  )
  results_list = lapply(
    X = ranges
    , FUN = function(x) {
      ids >= x[1] & ids <= x[2]
    }
  )
  results_df = do.call(
    what = rbind
    , args = results_list
  )
  results = apply(
    X = results_df
    , MARGIN = 2
    , FUN = any
  )
  sum(results)
}

count_fresh_ids(test) == 3

count_fresh_ids(input) 

count_possible_fresh = function(x) {
  blank = which(x == "")
  ids = as.numeric(x[(blank + 1):length(x)])
  ranges_list = lapply(
    X = x[1:(blank - 1)]
    , FUN = function(x) {
      from = regexpr("^[[:digit:]]+-", text = x)
      from = regmatches(x = x, m = from)
      from = gsub("-$", "", from)
      to = regexpr("-[[:digit:]]+$", text = x)
      to = regmatches(x = x, m = to)
      to = gsub("^-", "", to)
      as.numeric(c(from, to))
    }
  )
  ranges_df = do.call(
    what = rbind
    , args = unique(ranges_list)
  )
  while(TRUE) {
    new_ranges_list = lapply(
      X = 1:nrow(ranges_df)
      , FUN = function(i) {
        overlaps = ranges_df[i, 1] >= ranges_df[, 1] & ranges_df[i, 1] <= ranges_df[, 2]
        c(min(ranges_df[overlaps, 1]), max(ranges_df[overlaps, 2]))
      }
    )
    new_ranges_df = do.call(
      what = rbind
      , args = unique(new_ranges_list)
    )
    if (identical(new_ranges_df, ranges_df)) {
      break
    }
    ranges_df = new_ranges_df
  }
  range_lengths = apply(
    X = new_ranges_df
    , MAR = 1
    , FUN = function(x) {
      x[2] - x[1] + 1
    }
  )
  sum(range_lengths)
}

count_possible_fresh(test) == 14

count_possible_fresh(input)
