using BenchmarkTools, Test, SplitApplyCombine, IterTools, Match

small_input = open("small_input.txt") do f
    readlines(f)
end

input = open("input.txt") do f
    readlines(f)
end

function score(brackets, scores)
    return sum([scores[bracket] for bracket in brackets])
end

function parse(expr, stack, complete_expr=false)
    closing = Dict(
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>'
    )

    if expr == ""
        if complete_expr
            return join(reverse(stack))
        else
            return ""
        end
    elseif expr[1] ∈ ['(', '[', '{', '<']
        if length(expr) == 1
            push!(stack, closing[expr[1]])

            if complete_expr
                return join(reverse(stack))
            else
                return ""
            end
        else
            push!(stack, closing[expr[1]])

            return parse(expr[2:end], stack, complete_expr)
        end
    elseif expr[1] ∈ [')', ']', '}', '>']
        if length(stack) == 0 || expr[1] != pop!(stack)
            return expr[1]
        else
            return parse(expr[2:end], stack, complete_expr)
        end
    else
        error("Expected bracket, got $expr")
    end
end

function part1(input)
    errors = []
    scores = Dict(
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137
    )

    for line in input
        result = parse(line, [])

        if result ∉ ['(', '[', '{', '<'] && result != ""
            push!(errors, result)
        end
    end

    return score(errors, scores)
end

function score2(results, scoring)
    endresult = []

    for result in results
        push!(endresult, foldl((acc, el) -> acc * 5 + scoring[el], result, init=0))
    end

    return sort(endresult)[div(length(endresult), 2) + 1]
end

function part2(input)
    results = []
    scores = Dict(
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4
    )

    for line in input
        result = parse(line, [], true)

        if length(result) > 1
            push!(results, collect(result))
        end
    end

    return score2(results, scores)
end

# Complete
@test parse("([])", []) == ""
@test parse("{()()()}", []) == ""
@test parse("<([{}])>", []) == ""
@test parse("[<>({}){}[([])<>]]", []) == ""
@test parse("(((((((((())))))))))", []) == ""

# Incorrect
@test parse("(]", []) == ']'
@test parse("{()()()>", []) == '>'
@test parse("(((()))}", []) == '}'
@test parse("<([]){()}[{}])", []) == ')'

# Incomplete
@test parse("([])([", [], true) == "])"
@test parse("(((", [], true) == ")))"

@test part1(small_input) == 26397
part1(input)

# Incomplete
@test parse("([])([", [], true) == "])"
@test parse("[({(<(())[]>[[{[]{<()<>>", [], true) == "}}]])})]"
@test parse("[(()[<>])]({[<{<<[]>>(", [], true) == ")}>]})"
@test parse("(((({<>}<{<{<>}{[]{[]{}", [], true) == "}}>}>))))"
@test parse("{<[[]]>}<{[{[{[]{()[[[]", [], true) == "]]}}]}]}>"
@test parse("<{([{{}}[<[[[<>{}]]]>[]]", [], true) == "])}>"

@test part2(["[({(<(())[]>[[{[]{<()<>>"]) == 288957
@test part2(["[(()[<>])]({[<{<<[]>>("]) == 5566
@test part2(["(((({<>}<{<{<>}{[]{[]{}"]) == 1480781
@test part2(["{<[[]]>}<{[{[{[]{()[[[]"]) == 995444
@test part2(["<{([{{}}[<[[[<>{}]]]>[]]"]) == 294

@test part2(small_input) == 288957
part2(input)
