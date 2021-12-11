using BenchmarkTools, Test, SplitApplyCombine, IterTools, Match

small_input = open("small_input.txt") do f
    readlines(f)
end

input = open("input.txt") do f
    readlines(f)
end

function score(brackets)
    scores = Dict(
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137
    )

    return sum([scores[bracket] for bracket in brackets])
end

function parse(expr, stack)
    closing = Dict(
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>'
    )

    if expr == ""
        if length(stack) == 0
            return ""
        else
            return stack[end]
        end
    elseif expr[1] ∈ ['(', '[', '{', '<']
        if length(expr) == 1
            return expr[1]
        else
            push!(stack, closing[expr[1]])

            return parse(expr[2:end], stack)
        end
    elseif expr[1] ∈ [')', ']', '}', '>']
        if length(stack) == 0 || expr[1] != pop!(stack)
            return expr[1]
        else
            return parse(expr[2:end], stack)
        end
    else
        error("Expected bracket, got $expr")
    end
end

function part1(input)
    errors = []

    for line in input
        result = parse(line, [])

        if result ∈ ['(', '[', '{', '<']
            @info "SKIP" expr
            continue
        else
            push!(errors, result)
        end
    end

    @show errors

    return score(errors)
end

@test parse("([])", []) == ""
@test parse("{()()()}", []) == ""
@test parse("<([{}])>", []) == ""
@test parse("[<>({}){}[([])<>]]", []) == ""
@test parse("(((((((((())))))))))", []) == ""

@test parse("(]", []) == ']'
@test parse("{()()()>", []) == '>'
@test parse("(((()))}", []) == '}'
@test parse("<([]){()}[{}])", []) == ')'

@test part1(small_input) == 26397
