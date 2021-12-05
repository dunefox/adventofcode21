using Test

function read_file(name, delims)
    return open(name) do f
        map(l -> parse.(Int, l), split.(readlines(f), delims))
    end
end

small_input = read_file("small_input.txt", r"(,)|( -> )")
input = read_file("input.txt", r"(,)|( -> )")

function part1(input)
    counts = Dict{Array{Int}, Int}()

    for (x1, y1, x2, y2) in input
        if x1 == x2 && y1 != y2
            for coord in range(sort([y1, y2])...)
                counts[[x1, coord]] = get(counts, [x1, coord], 0) + 1
            end
        elseif x1 != x2 && y1 == y2
            for coord in range(sort([x1, x2])...)
                counts[[coord, y1]] = get(counts, [coord, y1], 0) + 1
            end
        end
    end

    return count(>(1), values(counts))
end

@test part1(small_input) == 5
part1(input)

# For testing purposes; getting the directions right ain't easy
function fill_board(counts)
    board = zeros(Int8, 10, 10)

    for ((x, y), v) in counts
        board[x + 1, y + 1] = v
    end

    display(board)
end

function part2(input)
    counts = Dict{Array{Int}, Int}()

    for (x1, y1, x2, y2) in input
        if x1 - x2 == y1 - y2
            for (coord1, coord2) in zip(range(sort([x1, x2])...), range(sort([y1, y2])...))
                counts[[coord1, coord2]] = get(counts, [coord1, coord2], 0) + 1
            end
        elseif abs(x1 - y2) == abs(x2 - y1)
            for (coord1, coord2) in zip(range(sort([x1, x2])...), reverse(range(sort([y1, y2])...)))
                counts[[coord1, coord2]] = get(counts, [coord1, coord2], 0) + 1
            end
        elseif x1 == x2 && y1 != y2
            for coord in range(sort([y1, y2])...)
                counts[[x1, coord]] = get(counts, [x1, coord], 0) + 1
            end
        elseif x1 != x2 && y1 == y2
            for coord in range(sort([x1, x2])...)
                counts[[coord, y1]] = get(counts, [coord, y1], 0) + 1
            end
        end
    end

    return count(>(1), values(counts))
end

@test part2(small_input) == 12
@time part2(input)
