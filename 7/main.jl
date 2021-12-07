using BenchmarkTools, Test

small_input = open("small_input.txt") do f
    parse.(Int, split(readlines(f)[1], ","))
end

input = open("input.txt") do f
    parse.(Int, split(readlines(f)[1], ","))
end

gauss(x::Int)::Int = (x^2 + x) / 2

function part1(crabs::Array{Int})
    _min = typemax(Int)

    for crab in crabs
        tmp = abs.(crabs .- crab) |> sum

        if tmp < _min
            _min = tmp
        end
    end

    return _min
end

function part2(crabs)
    _min = typemax(Int)

    for crab in crabs
        tmp = gauss.(abs.(crabs .- crab .- 1)) |> sum

        if tmp < _min
            _min = tmp
        end
    end

    return _min
end

@test part1(small_input) == 37
part1(input)
@test part2(small_input) == 168
part2(input)
