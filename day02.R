options(scipen = 9)

test = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124" |>
  gsub(
    pattern = "\n"
    , replacement = ""
  ) |>
  strsplit(split = ",") |>
  unlist()

input = readLines(con = "input/day02_input.txt") |>
  strsplit(split = ",") |>
  unlist()

extract_invalid_ids = function(x) {
  id_range = strsplit(x, "-")[[1]]
  ids = data.frame(
    sequence = seq(from = id_range[1], to = id_range[2])
  )
  ids = within(
    ids
    , {
      n_characters = nchar(sequence)
      first = substr(
        x = as.character(sequence)
        , start = 1
        , stop = floor(n_characters) / 2
      )
      second = substr(
        x = as.character(sequence)
        , start = floor(n_characters) / 2 + 1
        , stop = n_characters
      )
      match = first == second
    }
  )
  ids$sequence[ids$match]
}

test_invalid_ids = sapply(
  X = test
  , FUN = extract_invalid_ids
)
sum(unlist(test_invalid_ids))

system.time({
  invalid_ids = sapply(
    X = input
    , FUN = extract_invalid_ids
  )
  print(sum(unlist(invalid_ids)))
})

extract_invalid_ids2 = function(x) {
  id_range = strsplit(x, "-")[[1]]
  sequence = seq(from = id_range[1], to = id_range[2])
  n_characters = nchar(sequence)
  pattern_max = floor(n_characters / 2)
  invalid_sequence_list = lapply(
    X = seq(1, max(pattern_max))
    , FUN = function(n) {
      pattern = substr(x = sequence, start = 1, stop = n)
      invalid_sequence = sapply(
        X = seq_along(pattern)
        , FUN = function(i) {
          as.numeric(
            paste0(rep(pattern[i], each = n_characters[i] / n), collapse = "")
          )
        }
      )
      sequence[sequence == invalid_sequence]
    }
  )
  result = unique(unlist(invalid_sequence_list))
  result[nchar(result) > 1]
}

test_invalid_ids2 = lapply(
  X = test
  , FUN = extract_invalid_ids2
)
sum(as.numeric(unlist(test_invalid_ids2))) == 4174379265

invalid_ids2 = lapply(
  X = input
  , FUN = extract_invalid_ids2
)
sum(as.numeric(unlist(invalid_ids2))) == 30962646823
