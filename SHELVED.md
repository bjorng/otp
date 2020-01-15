Why shelved
===========

The idea to do a topological sort to allow binding binary size
variables and map keys in parallel to its use has been shelved.

Here is why.

Many things work as long as no variables are repeated. For example,
the following works:

    convoluted(Ref,
           #{ node(Ref) := NodeId, Loop := universal_answer},
           [{NodeId, Size} | T],
           <<Int:(Size*8+length(T)),Loop>>) when is_reference(Ref) ->
        Int.

More examples of things that work can be found at the end of
`match_SUITE`.

Here follows a list of examples of things that don't work.

It is unclear how much that would be needed to make everything work.
It seems that most of the work would be in `erl_lint`. `v3_core` has
an easier task in that it can assume that the code is correct.

The following example should give an compilation error saying that
`K` is undefined.

    foo(#{K := K}) -> ok.

The compiler currently gives the following message:

    the following variables are mutually dependent: K

The following example should work, but the `erl_lint` rejects it
with the same message as for the previous example:

    foo(#{K := K}, K) -> ok.

`v3_core` also fails to compile this example.

The example passes when rewritten like this:

    foo(#{K := V}, K) when K =:= V -> ok.

Here is bunch of other examples that should work, but which `erl_lint`
rejects:

    bin2(<<Sz:8,X:Sz>>, <<Y:Sz>>) -> {X,Y}.

    repeated_vars(#{K := #{K := K}}, K) -> K.

    match_map_bs(#{K1 := {bin,<<Int:Sz>>}, K2 := <<Sz:8>>}, {K1,K2}) -> Int.

    bar(#{B := X}, {<<B:Sz>>,Sz}) ->
        {Sz,B,X}.

    deeper_bar(#{B := X}, {<<B:Sz>>,{tagged,Sz}}) ->
        {Sz,B,X}.

    even_deeper_bar([#{B := X}, {<<B:Sz>>,{tagged,Sz}}]) ->
        {Sz,B,X}.

    bazzier(#{{key,K1} := <<Result:32>>, size_key := Sz, {key,K2} := {bin,<<K1:Sz>>}}, K2) ->
        Result.

