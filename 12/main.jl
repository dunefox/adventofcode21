using Test, BenchmarkTools, IterTools, SplitApplyCombine, Match, Query

small_input = open("small_input.txt") do f
    split.(readlines(f), "-")
end

middle_input = open("middle_input.txt") do f
    split.(readlines(f), "-")
end

large_input = open("large_input.txt") do f
    split.(readlines(f), "-")
end

input = open("small_input.txt") do f
    split.(readlines(f), "-")
end

function part1(input)
    visited = []
    counter = 0
    
end

@test part1(small_input) == 10
@test part1(middle_input) == 19
@test part1(large_input) == 226
