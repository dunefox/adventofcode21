using BenchmarkTools, Test, IterTools, SplitApplyCombine, Query, Match, Underscores, DataStructures

small_input = open("small_input.txt") do f
    @_ readlines(f) |> collect.(__) |> map(parse.(Int, _), __) |> (reduce(hcat, __)) |> transpose
end

input = open("input.txt") do f
    @_ readlines(f) |> collect.(__) |> map(parse.(Int, _), __) |> (reduce(hcat, __)) |> transpose
end

function euclidian_distance(start, goal)
    return sum(goal .- start)
end

function neighbours(pos, dims)
    return filter(
        x -> 1 <= x[1] <= dims[1] && 1 <= x[2] <= dims[2],
        [pos + n for n in [[0, 1], [1, 0], [-1, 0], [0, -1]]]
    )
end

# Only needed for debugging
function reconstruct_path(camefrom, current)
    total_path = [current]

    while current ∈ keys(camefrom)
        current = camefrom[current]
        push!(total_path, current)
    end

    return reverse(total_path)
end

function a_star(field, start, goal, h)::Int
    openset = PriorityQueue()
    camefrom = Dict()
    gscore = DefaultDict(Inf)
    fscore = DefaultDict(Inf)
    gscore[start] = 0
    fscore[start] = h(start, goal)

    enqueue!(openset, start, 0)

    while length(openset) > 0
        current = dequeue!(openset)

        if current == collect(goal)
            return gscore[current]
            # Path isn't needed here:
            # return reconstruct_path(camefrom, current)
        end

        for neighbour in neighbours(current, goal)
            gscoreᵢ::Int = gscore[current] + field[neighbour...]

            if gscoreᵢ < gscore[neighbour]
                camefrom[neighbour] = current
                gscore[neighbour] = gscoreᵢ
                fscore[neighbour] = gscoreᵢ + h(neighbour, goal)
            
                if neighbour ∉ keys(openset)
                    enqueue!(openset, neighbour, fscore[neighbour])
                end
            end
        end
    end

    error("Invalid state")
end

function part1(input)
    start = [1, 1]
    goal = size(input)
    
    return a_star(input, start, goal, euclidian_distance)::Int
end

@test part1(small_input) == 40
part1(input)
