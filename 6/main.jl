using Pkg
Pkg.activate(".")

using BenchmarkTools, Test

small_input = parse.(Int8, split(readlines(open("small_input.txt"))[1], ","))
input = parse.(Int8, split(readlines(open("input.txt"))[1], ","))

function adapt_population!(population)
    population[8] += population[1]
    population .= circshift(population, -1)
end

function part12(input, days)
    fishies = zeros(Int, 9)

    map(x -> fishies[x] += 1, input .+ 1) # Julia indices are 1-based

    for day in 1:days
        adapt_population!(fishies)
    end

    return sum(fishies)
end

# Tests
@test part12(small_input, 80) == 5934
@test part12(small_input, 256) == 26984457539
println(part12(input, 80))
println(part12(input, 256))

# Benchmarks
@benchmark part12(small_input, 80)
@benchmark part12(input, 80)
@benchmark part12(small_input, 256)
@benchmark part12(input, 256)
