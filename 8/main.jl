using BenchmarkTools, Test, IterTools, SplitApplyCombine

small_input = open("small_input.txt") do f
    split.(readlines(f), r"( \| )|( )")
end

function get_diff(word1, word2)
    setdiff(Set(word1...), Set(word2...))
end

function part2(input)
    input = input[1]
    
    mapping = Dict()
    left = IterTools.take(input, 10)
    right = IterTools.drop(input, 10) |> x -> IterTools.take(x, 4)
    groups = group(length, left)
    
    mapping["top"] = get_diff(groups[3], groups[2]) # d
    
end

part2(small_input)
