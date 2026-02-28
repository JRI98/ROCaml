last : List(a) -> Try(a, [ListWasEmpty])
last = |l| {
    match l {
        [] => Err(ListWasEmpty)
        [.., e] => Ok(e)
    }
}

expect
    last(["a", "b", "c", "d"]) == Ok("d")

expect
    last([]) == Err(ListWasEmpty)

last_two : List(a) -> Try((a, a), [ListLengthLessThanTwo])
last_two = |l| {
    match l {
        [.., e1, e2] => Ok((e1, e2))
        _ => Err(ListLengthLessThanTwo)
    }
}

expect
    last_two(["a", "b", "c", "d"]) == Ok(("c", "d"))

expect
    last_two(["a"]) == Err(ListLengthLessThanTwo)

nth : List(a), U64 -> Try(a, [OutOfBounds])
nth = |l, i| {
    match (l, i) {
        ([], _) => Err(OutOfBounds)
        ([e, ..], 0) => Ok(e)
        ([_, .. as rest], _) => nth(rest, (i - 1))
    }
}

expect
    nth(["a", "b", "c", "d", "e"], 2) == Ok("c")

expect
    nth(["a"], 2) == Err(OutOfBounds)

length : List(a) -> U64
length = |input| {
    length_aux = |l, acc| {
        match l {
            [] => acc
            [_, .. as rest] => length_aux(rest, (acc + 1))
        }
    }
    length_aux(input, 0)
}

expect
    length(["a", "b", "c"]) == 3

expect
    length([]) == 0

rev : List(a) -> List(a)
rev = |input| {
    rev_aux = |l, acc| {
        match l {
            [] => acc
            [e, .. as rest] => rev_aux(rest, List.concat([e], acc))
        }
    }
    rev_aux(input, [])
}

expect
    rev(["a", "b", "c"]) == ["c", "b", "a"]

is_palindrome : List(a) -> Bool where [a.is_eq : a, a -> Bool]
is_palindrome = |l| {
    l == rev(l)
}

expect
    is_palindrome(["x", "a", "m", "a", "x"]) == Bool.True

expect
    Bool.not(is_palindrome(["a", "b"])) == Bool.True

#Node(a) := [One(a), Many(List(Node(a)))]
#
#flatten : List(Node(a)) -> List(a)
#flatten = |input| {
#    flatten_aux = |l, acc| {
#        match l {
#            [] => acc
#            [One(e), .. as rest] => flatten_aux(rest, List.append(acc, e))
#            [Many(e), .. as rest] => flatten_aux(rest, flatten_aux(e, acc))
#        }
#    }
#    flatten_aux(input, [])
#}
#
#expect
#    flatten([One("a"), Many([One("b"), Many([One("c"), One("d")]), One("e")])]) == ["a", "b", "c", "d", "e"]

#compress : List(a) -> List(a) where [a.is_eq : a, a -> Bool]
#compress = |l| {
#    match l {
#        [] => []
#        [e] => [e]
#        [e1, e2, .. as rest] => {
#            rest_compression = compress(List.concat([e2], rest))
#            if e1 == e2 { rest_compression } else { List.concat([e1], rest_compression) }
#        }
#    }
#}
#
#expect
#    compress(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == ["a", "b", "c", "a", "d", "e"]

#pack : List(a) -> List(List(a)) where [a.is_eq : a, a -> Bool]
#pack = |input| {
#    pack_aux = |l, acc| {
#        match l {
#            [] => acc
#            [e, .. as rest] => {
#                rest_pack = pack_aux(rest, [])
#                match rest_pack {
#                    [] => [[e]]
#                    [[rpe, ..] as p, .. as rprest] => {
#                        if e == rpe {
#                            List.concat([List.append(p, e)], rprest)
#                        } else {
#                            List.concat([[e]], rest_pack)
#                        }
#                    }
#
#                    _ => {
#                        crash "unreachable"
#                    }
#                }
#            }
#        }
#    }
#    pack_aux(input, [])
#}
#
#expect
#    pack(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "d", "e", "e", "e", "e"]) == [["a", "a", "a", "a"], ["b"], ["c", "c"], ["a", "a"], ["d", "d"], ["e", "e", "e", "e"]]

#encode1 : List(a) -> List((U64, a)) where [a.is_eq : a, a -> Bool]
#encode1 = |input| {
#    List.map(
#        pack(input),
#        |l| {
#            match l {
#                [e, ..] => (length(l), e)
#                _ => {
#                    crash "unreachable"
#                }
#            }
#        },
#    )
#}
#
#expect
#    encode1(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == [(4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")]

#Rle(a) : [One(a), Many(U64, a)]
#
#encode2 : List(a) -> List(Rle(a)) where [a.is_eq : a, a -> Bool]
#encode2 = |input| {
#    List.map(
#        pack(input),
#        |l| {
#            match l {
#                [e, ..] => {
#                    len = length(l)
#                    if len == 1 {
#                        One(e)
#                    } else {
#                        Many((len, e))
#                    }
#                }
#                _ => {
#                    crash "unreachable"
#                }
#            }
#        },
#    )
#}
#
#expect
#    encode2(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == [Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))]

#decode : List(Rle(a)) -> List(a)
#decode = |l| {
#    List.join_map(
#        l,
#        |rle| {
#            match rle {
#                One(e) => [e]
#                Many((n, e)) => List.repeat(e, n)
#            }
#        },
#    )
#}
#
#expect
#    decode([Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))]) == ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]

#encode3 : List(a) -> List(Rle(a)) where [a.is_eq : a, a -> Bool]
#encode3 = |l| {
#    match l {
#        [] => []
#        [e, .. as rest] => {
#            encode_rest = encode3(rest)
#            match encode_rest {
#                [] => [One(e)]
#                [er, .. as errest] => {
#                    (element, number) =
#                        match er {
#                            One(el) => (el, 1)
#                            Many((n, el)) => (el, n)
#                        }
#
#                    if e == element {
#                        List.concat([Many((number + 1, e))], errest)
#                    } else {
#                        List.concat([One(e)], encode_rest)
#                    }
#                }
#            }
#        }
#    }
#}
#
#expect
#    encode3(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]) == [Many((4, "a")), One("b"), Many((2, "c")), Many((2, "a")), One("d"), Many((4, "e"))]

duplicate : List(a) -> List(a)
duplicate = |l| {
    match l {
        [] => []
        [e, .. as rest] => {
            List.concat([e, e], duplicate(rest))
        }
    }
}

expect
    duplicate(["a", "b", "c", "c", "d"]) == ["a", "a", "b", "b", "c", "c", "c", "c", "d", "d"]

replicate : List(a), U64 -> List(a)
replicate = |l, n| {
    match l {
        [] => []
        [x, .. as rest] => List.concat(List.repeat(x, n), replicate(rest, n))
    }
}

expect
    replicate(["a", "b", "c"], 3) == ["a", "a", "a", "b", "b", "b", "c", "c", "c"]

drop : List(a), U64 -> List(a)
drop = |input, number| {
    drop_aux : List(a), U64, List(a) -> List(a)
    drop_aux = |l, n, acc| {
        match l {
            [] => acc
            [x, .. as rest] => {
                if n == 1 {
                    drop_aux(rest, number, acc)
                } else {
                    drop_aux(rest, (n - 1), List.append(acc, x))
                }
            }
        }
    }

    drop_aux(input, number, [])
}

expect
    drop(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3) == ["a", "b", "d", "e", "g", "h", "j"]

split : List(a), U64 -> (List(a), List(a))
split = |l, i| {
    match (l, i) {
        ([], _) => ([], [])
        (_, 0) => ([], l)
        ([x, .. as rest], _) => {
            match split(rest, (i - 1)) {
                (left, right) => (List.concat([x], left), right)
            }
        }
    }
}

expect
    split(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 3) == (["a", "b", "c"], ["d", "e", "f", "g", "h", "i", "j"])
expect
    split(["a", "b", "c", "d"], 5) == (["a", "b", "c", "d"], [])

slice : List(a), U64, U64 -> List(a)
slice = |l, start, end| {
    match l {
        [] => []
        [x, .. as rest] => {
            if start > 0 {
                slice(rest, (start - 1), (end - 1))
            } else if end == 0 {
                [x]
            } else {
                List.concat([x], slice(rest, 0, (end - 1)))
            }
        }
    }
}

expect
    slice(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"], 2, 6) == ["c", "d", "e", "f", "g"]
