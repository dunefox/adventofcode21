using BenchmarkTools, Test, IterTools, SplitApplyCombine, Match, Query, Underscores

function remove_prefix(s, prefix)
    if startswith(s, prefix)
        return s[length(prefix) + 1:end]
    else
        return s
    end
end

function parse_input(file)
    open(file) do f
        vals, instr = group(x -> startswith(x, "fold"), readlines(f))
        vals = @_ vals |>
            split.(__, ",") |>
            map(parse.(Int, _), __)
        instr = @_ instr |>
            remove_prefix.(__, "fold along ") |>
            split.(__, "=") |>
            map([_[1], parse(Int, _[2])], __)
    
        vals, instr
    end
end

small_values, small_instructions = parse_input("small_input.txt")
values, instructions = parse_input("input.txt")

function compare(a, b, at, direction)
    if direction == "x"
        if a > at
            return [at * 2 - a, b]
        end
    else
        if b > at
            return [a, at * 2 - b]
        end
    end

    return [a, b]
end

one_iteration(values, coord, direction) = @_ map(compare(_[1], _[2], coord, direction), values) |> unique

function part1(values, instructions)
    direction::String, coord::Int = instructions[1]

    one_iteration(values, coord, direction)
end

function part2(values, instructions)
    values = copy(values)

    for (direction::String, coord::Int) in instructions
        values = one_iteration(values, coord, direction)
    end

    return values
end

@test part1(small_values, small_instructions) |> length == 17
part1(values, instructions) |> length
@test part2(small_values, small_instructions) |> length == 16
part2(values, instructions)
