using BenchmarkTools, Test, SplitApplyCombine, IterTools

small_input = open("small_input.txt") do f
    transpose(reduce(hcat, [parse.(Int, collect(line)) for line in readlines(f)]))
end

input = open("input.txt") do f
    transpose(reduce(hcat, [parse.(Int, collect(line)) for line in readlines(f)]))
end

# Squids aren't independent of each other since activations propagate to previously seen Squids
# -> outward propagation, not inward condition like in game of life
# Approach: Iterate over squids and propagate activations via flood fill?
# Problem: Squids can be reactivated, visited squids need to be revisited if over threshold

function generate_neighbours(I, R)
    Itop = CartesianIndex(1, 0)
    Ileft = CartesianIndex(0, 1)

    neighbours =[
        # direct
        I - Itop,
        I - Ileft,
        I + Itop,
        I + Ileft,
        # diagonal
        I - (Itop + Ileft),
        I + Itop + Ileft,
        I + Itop - Ileft,
        I - Itop + Ileft,
    ]

    return filter(C -> C ∈ R, neighbours) # valid neighbours    
end

function inc!(m)
    m .+= ones(Int, size(m))
end

function inc!(m, index, value)
    m[index] += value
end

reset!(m, positions) = m .*= (positions .== 0)

function part1(input)
    current_map = copy(input)
    R = CartesianIndices(current_map)
    c = CartesianIndex
    cumsum = 0

    for step in 1:100
        inc!(current_map)
        actives = current_map .> 9

        if any(==(1), actives)
            current_neighbours = flatten([generate_neighbours(x, R) for x in filter(x -> x != c(0, 0), R .* actives)])

            for neighbour in current_neighbours
                inc!(current_map, neighbour, 1)
            end

            reset!(current_map, actives)
            new_actives = current_map .> 9

            while any(==(1), new_actives)
                current_neighbours = flatten([generate_neighbours(x, R) for x in filter(x -> x != c(0, 0), R .* new_actives)])

                for neighbour in current_neighbours
                    inc!(current_map, neighbour, 1)
                end

                actives += new_actives
                reset!(current_map, actives)
                new_actives = current_map .> 9
            end
        end

        cumsum += sum(actives .>= 1)
    end

    return cumsum
end

@test part1(small_input) == 1656
part1(input)

function part2(input)
    current_map = copy(input)
    R = CartesianIndices(current_map)
    c = CartesianIndex
    cumsum = 0

    for step in 1:100
        inc!(current_map)
        actives = current_map .> 9

        if any(==(1), actives)
            current_neighbours = flatten([generate_neighbours(x, R) for x in filter(x -> x != c(0, 0), R .* actives)])

            for neighbour in current_neighbours
                inc!(current_map, neighbour, 1)
            end

            reset!(current_map, actives)
            new_actives = current_map .> 9

            while any(==(1), new_actives)
                current_neighbours = flatten([generate_neighbours(x, R) for x in filter(x -> x != c(0, 0), R .* new_actives)])

                for neighbour in current_neighbours
                    inc!(current_map, neighbour, 1)
                end

                actives += new_actives
                reset!(current_map, actives)
                new_actives = current_map .> 9
            end
        end

        cumsum += sum(actives .>= 1)
    end

    return cumsum
end

@test part1(small_input) == 1656
part1(input)
