options(scipen = 9)

test = "987654321111111
811111111111119
234234234234278
818181911112111" |>
  strsplit("\n") |>
  unlist()

input = readLines("input/day03_input.txt")

calculate_joltages = function(x) {
  s = strsplit(x, "")[[1]]
  j_list = sapply(
    X = seq_along(s)[-length(s)]
    , FUN = function(i) {
      as.numeric(paste0(s[i], s[(i + 1):length(s)]))
    }
  )
  max(unlist(j_list))
}

joltages_list_test = lapply(
  X = test
  , FUN = calculate_joltages
)
sum(unlist(joltages_list_test)) == 357

joltages_list = lapply(
  X = input
  , FUN = calculate_joltages
)
sum(unlist(joltages_list))

calculate_joltages2 = function(x) {
  out = character(12)
  s = strsplit(x, "")[[1]]
  for (i in seq_along(out)) {
    prefix = s[1:(length(s) - 12 + i)]
    biggest = which(prefix == max(prefix))[1]
    out[i] = s[biggest]
    s = s[(biggest + 1):length(s)]
  }
  as.numeric(paste(out, collapse = ""))
}

joltages_list_test2 = lapply(
  X = test
  , FUN = calculate_joltages2
)
sum(unlist(joltages_list_test2)) == 3121910778619

joltages_list2 = lapply(
  X = input
  , FUN = calculate_joltages2
)
sum(unlist(joltages_list2))
