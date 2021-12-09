using BenchmarkTools, IterTools, SplitApplyCombine, Test

small_input = open("small_input.txt") do f
    map(x -> parse.(Int, [x...]), readlines(f)) |> x -> transpose(reduce(hcat, x))
end

input = open("input.txt") do f
    map(x -> parse.(Int, [x...]), readlines(f)) |> x -> transpose(reduce(hcat, x))
end

function generate_neighbours(I, R)
    Itop = CartesianIndex(1, 0)
    Ileft = CartesianIndex(0, 1)

    return filter(C -> C ∈ R, [I - Itop, I - Ileft, I + Itop, I + Ileft]) # valid neighbours    
end

function part1(input)
    R = CartesianIndices(input)
    poi = [1]         # I don't know how else to get an Array{Int}
    deleteat!(poi, 1) # ^

    for I in R
        N = generate_neighbours(I, R)

        if all(>(input[I]), [input[n] for n in N])
            push!(poi, input[I])
        end
    end

    return sum(poi .+ 1)
end

function current_step!(visited, edge, R, basin_map)
    current = pop!(edge)
    neighbours = generate_neighbours(current, R)
    
    push!(visited, current)

    for neighbour in neighbours
        # Neighbour can't be already visited, in the edges, or have a value of 1 to exclude basin walls (value 9)
        if neighbour ∉ visited && neighbour ∉ edge && basin_map[neighbour] == 0
            push!(edge, neighbour)
        end
    end
end

function part2(input)
    R = CartesianIndices(input)
    Basins = [] # Deepest points
    
    for I in R
        N = generate_neighbours(I, R)

        if all(>(input[I]), [input[n] for n in N])
            push!(Basins, I)
        end
    end
    
    sizes = [1]          # The things one does for an Int-Vector... 
    deleteat!(sizes, 1)  # Seriously, I should look up the correct instantiation :^)
    basin_map = input .== 9

    for point::CartesianIndex{2} in Basins
        visited = []
        edge = [point]

        while length(edge) > 0
            current_step!(visited, edge, R, basin_map)
        end

        push!(sizes, length(visited))
    end

    return reduce(*, sort(sizes, rev=true)[1:3])
end

@test part1(small_input) == 15
part1(input)

@test part2(small_input) == 1134
part2(input)
