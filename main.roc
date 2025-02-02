app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

last : List a -> Result a [ListWasEmpty]
last = |l|
    when l is
        [] -> Err(ListWasEmpty)
        [.., e] -> Ok(e)

# last = \l -> List.last l

expect
    last(["a", "b", "c", "d"]) == Ok("d")

expect
    last([]) == Err(ListWasEmpty)

last_two : List a -> Result (a, a) [ListLengthLessThanTwo]
last_two = |l|
    when l is
        [.., e1, e2] -> Ok((e1, e2))
        _ -> Err(ListLengthLessThanTwo)

# lastTwo = \l ->
#     when List.takeLast l 2 is
#         [a, b] -> Ok (a, b)
#         _ -> Err ListLengthLessThanTwo

expect
    last_two(["a", "b", "c", "d"]) == Ok(("c", "d"))

expect
    last_two(["a"]) == Err(ListLengthLessThanTwo)

nth : List a, U64 -> Result a [OutOfBounds]
nth = |l, i|
    when (l, i) is
        ([], _) -> Err(OutOfBounds)
        ([e, ..], 0) -> Ok(e)
        ([_, .. as rest], _) -> nth(rest, (i - 1))

# nth = \l, i -> List.get l i

expect
    nth(["a", "b", "c", "d", "e"], 2) == Ok("c")

expect
    nth(["a"], 2) == Err(OutOfBounds)

length : List a -> U64
length = |input|
    length_aux = |l, acc|
        when l is
            [] -> acc
            [_, .. as rest] -> length_aux(rest, (acc + 1))
    length_aux(input, 0)

# length = \l -> List.len l

expect
    length(["a", "b", "c"]) == 3

expect
    length([]) == 0

rev : List a -> List a
rev = |input|
    rev_aux = |l, acc|
        when l is
            [] -> acc
            [e, .. as rest] -> rev_aux(rest, List.prepend(acc, e))
    rev_aux(input, [])

# rev = \input -> List.reverse input

expect
    rev(["a", "b", "c"]) == ["c", "b", "a"]

is_palindrome : List a -> Bool where a implements Eq
is_palindrome = |l|
    l == rev(l)

# isPalindrome = \l -> l == List.reverse l

expect
    is_palindrome(["x", "a", "m", "a", "x"]) == Bool.true

expect
    Bool.not(is_palindrome(["a", "b"])) == Bool.true

Node a : [One a, Many (List (Node a))]

flatten : List (Node a) -> List a
flatten = |input|
    flatten_aux = |l, acc|
        when l is
            [] -> acc
            [One(e), .. as rest] -> flatten_aux(rest, List.append(acc, e))
            [Many(e), .. as rest] -> flatten_aux(rest, flatten_aux(e, acc))
    flatten_aux(input, [])

# flatten = \l ->
#     List.walk
#         l
#         []
#         (\acc, e ->
#             when e is
#                 One x -> List.append acc x
#                 Many x -> List.concat acc (flatten x)
#         )

expect
    flatten([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) == ["a", "b", "c", "d", "e"]

compress : List a -> List a where a implements Eq
compress = |l|
    when l is
        [] -> []
        [e] -> [e]
        [e1, e2, .. as rest] ->
            rest_compression = compress(List.prepend(rest, e2))
            if e1 == e2 then rest_compression else List.prepend(rest_compression, e1)

# compress = \l ->
#     List.walk
#         l
#         []
#         (\acc, e ->
#             when acc is
#                 [] -> [e]
#                 [.., x] -> if e == x then acc else List.append acc e
#         )

expect
    compress(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == ["a", "b", "c", "a", "d", "e"]

pack : List a -> List (List a) where a implements Eq
pack = |input|
    pack_aux = |l, acc|
        when l is
            [] -> acc
            [e, .. as rest] ->
                rest_pack = pack_aux(rest, [])
                when rest_pack is
                    [] -> [[e]]
                    [[rpe, ..] as p, .. as rprest] ->
                        if e == rpe then List.prepend(rprest, List.append(p, e)) else List.prepend(rest_pack, [e])

                    _ -> crash("unreachable")
    pack_aux(input, [])

expect
    pack(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "d", "e", "e", "e", "e"]) == [["a", "a", "a", "a"], ["b"], ["c", "c"], ["a", "a"], ["d", "d"], ["e", "e", "e", "e"]]

encode1 : List a -> List (U64, a) where a implements Eq
encode1 = |input|
    pack(input)
    |> List.map(
        |l|
            when l is
                [e, ..] -> (length(l), e)
                _ -> crash("unreachable"),
    )

expect
    encode1(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == [(4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")]

Rle a : [One a, Many (U64, a)]

encode2 : List a -> List (Rle a) where a implements Eq
encode2 = |input|
    pack(input)
    |> List.map(
        |l|
            when l is
                [e, ..] ->
                    len = length(l)
                    if len == 1 then
                        One(e)
                    else
                        Many((len, e))

                _ -> crash("unreachable"),
    )

expect
    encode2(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == [Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))]

decode : List (Rle a) -> List a
decode = |l|
    List.join_map(
        l,
        |rle|
            when rle is
                One(e) -> [e]
                Many((n, e)) -> List.repeat(e, n),
    )

expect
    decode([Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))]) == ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]

encode3 : List a -> List (Rle a) where a implements Eq
encode3 = |l|
    when l is
        [] -> []
        [e, .. as rest] ->
            encode_rest = encode3(rest)
            when encode_rest is
                [] -> [One(e)]
                [er, .. as errest] ->
                    (element, number) =
                        when er is
                            One(el) -> (el, 1)
                            Many((n, el)) -> (el, n)

                    if e == element then List.prepend(errest, Many((number + 1, e))) else List.prepend(encode_rest, One(e))

expect
    encode3(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == [Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))]

duplicate : List a -> List a
duplicate = |l|
    when l is
        [] -> []
        [e, .. as rest] ->
            [e, e]
            |> List.concat(duplicate(rest))

expect
    duplicate(["a", "b", "c", "c", "d"]) == ["a", "a", "b", "b", "c", "c", "c", "c", "d", "d"]

replicate : List a, U64 -> List a
replicate = |l, n|
    when l is
        [] -> []
        [x, .. as rest] -> List.concat(List.repeat(x, n), replicate(rest, n))

expect
    replicate(["a", "b", "c"], 3) == ["a", "a", "a", "b", "b", "b", "c", "c", "c"]

drop : List a, U64 -> List a
drop = |input, number|
    drop_aux : List a, U64, List a -> List a
    drop_aux = |l, n, acc|
        when l is
            [] -> acc
            [x, .. as rest] ->
                if n == 1 then
                    drop_aux(rest, number, acc)
                else
                    drop_aux(rest, (n - 1), List.append(acc, x))

    drop_aux(input, number, [])

expect
    drop(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3) == ["a", "b", "d", "e", "g", "h", "j"]

split : List a, U64 -> (List a, List a)
split = |l, i|
    when (l, i) is
        ([], _) -> ([], [])
        (_, 0) -> ([], l)
        ([x, .. as rest], _) ->
            when split(rest, (i - 1)) is
                (left, right) -> (List.prepend(left, x), right)

expect
    split(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3) == (["a", "b", "c"], ["d", "e", "f", "g", "h", "i", "j"])
expect
    split(["a", "b", "c", "d"], 5) == (["a", "b", "c", "d"], [])

slice : List a, U64, U64 -> List a
slice = |l, start, end|
    when l is
        [] -> []
        [x, .. as rest] ->
            if start > 0 then
                slice(rest, (start - 1), (end - 1))
            else if end == 0 then
                [x]
            else
                List.prepend(slice(rest, 0, (end - 1)), x)

expect
    slice(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 2, 6) == ["c", "d", "e", "f", "g"]

main! =
    crash(sink([A(nth), B(compress), C(encode3), D(duplicate), E(replicate), F(split), G(slice)])) # Omit unused warnings for recursive functions

sink = |_| ""
