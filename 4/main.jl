using Test

function read_file(name, delim)
    if endswith(name, "numbers.txt")
        return open(name) do f
            parse.(Int32, split(readlines(f)[1], delim))
        end
    else
        return open(name) do f
            [parse.(Int16, x) for x in split.(readlines(f), delim)]
        end
    end
end

small_cards = read_file("small_cards.txt", " ")
small_numbers = read_file("small_numbers.txt", ",")
cards = read_file("cards.txt", " ")
numbers = read_file("numbers.txt", ",")

function solved(all, return_index=false)
    for (i, (card, indicator)) in enumerate(all)
        cols, rows = sum.([eachcol(indicator); eachrow(indicator)])

        if 5 ∈ cols || 5 ∈ rows
            return !return_index ? [true, card, indicator] : [true, card, indicator, i]
        end
    end

    return false, nothing, nothing, nothing
end

function summed_entries(card, indicator)
    return sum(card .* (indicator .== 0))
end

function part1(cards, numbers)
    matrices = [transpose(hcat(x...)) for x in Iterators.partition(cards, 5)]
    indicators = [zeros(Int8, 5, 5) for _ in matrices]
    all = collect(zip(matrices, indicators))
    
    for (i, number) in enumerate(numbers)
        done, solved_card, solved_indicator = solved(all)

        if done
            return summed_entries(solved_card, solved_indicator) * numbers[i - 1]
        else
            for (card, indicator) in filter(x -> number ∈ x[1], all)
                indicator .+= (card .== number)
            end
        end
    end
end

# Part 1: naive solution with unnecessary allocations, but with some cool Julia-Fu :^)
# Broadcasting with matrices is pretty nice for this puzzle:
#   indicator .+= (card .== number)
#   sum(card .* (indicator .== 0))
# Possibly better to solve cards individually and break if number of steps exceeds the prior minimum
@time part1(small_cards, small_numbers) == 4512
@time part1(cards, numbers)

function part2(cards, numbers)
    matrices = [transpose(hcat(x...)) for x in Iterators.partition(cards, 5)]
    indicators = [zeros(Int8, 5, 5) for _ in matrices]
    all = collect(zip(matrices, indicators))
    
    for number in numbers
        for (card, indicator) in filter(x -> number ∈ x[1], all)
            indicator .+= (card .== number)
        end

        done, solved_card, solved_indicator, index = solved(all, true)

        if done && length(all) == 1
            res = summed_entries(solved_card, solved_indicator) * number

            return res
        elseif done
            deleteat!(all, index)
        end
    end
end

@time part2(small_cards, small_numbers) == 1924
@time part2(cards, numbers)
