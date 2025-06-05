-module(qr).
-export([generate_qr/2]).
-on_load(init/0).

init() ->
    io:format("Loading NIF...~n"),
    Result = erlang:load_nif("priv/libqr", 0),
    io:format("Load result: ~p~n", [Result]),
    Result.

generate_qr_nif(_TextToEncode, _Size) ->
    exit(nif_library_not_loaded).

generate_qr(TextToEncode, Size) ->
    generate_qr_nif(TextToEncode, Size).
