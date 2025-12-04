test = strsplit(x = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
, split = "\n")[[1]]

input = readLines(con = "input/day01_input.txt")

aoc2025_day1 = function(x, start) {
    rotations = as.numeric(gsub("R", "", gsub("L", "-", x)))
    sum(cumsum(c(start, rotations))[-1] %% 100 == 0)
}

aoc2025_day1(test, 50)

aoc2025_day1(input, 50)

aoc2025_day1_part2 = function(x, start) {    
    rotations = as.numeric(gsub("R", "", gsub("L", "-", x)))
    positions = cumsum(c(start, rotations))
    pass_zero = sapply(
        X = seq_along(positions)[-length(positions)]
      , FUN = function(i) {
          all_pos = seq(from = positions[i], to = positions[i + 1])[-1] %% 100
          sum(all_pos == 0)
        }
    )
    sum(pass_zero)
}

aoc2025_day1_part2(test, 50)

aoc2025_day1_part2("R1000", 50)

aoc2025_day1_part2(input, 50)
