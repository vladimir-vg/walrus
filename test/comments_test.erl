-module(comments_test).

-compile([export_all]).

comments_test() ->
    Examples = test_helpers:read_examples("../mustache/specs/comments.json"),
    test_helpers:run_examples(Examples).
