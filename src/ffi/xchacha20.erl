-module(xchacha20).
-export([generate_key/0, generate_nonce/0, encrypt/3, decrypt/3]).
-on_load(init/0).

init() ->
    io:format("Loading NIF...~n"),
    Result = erlang:load_nif("priv/libxchacha20", 0),
    io:format("Load result: ~p~n", [Result]),
    Result.

generate_key() ->
    exit(nif_library_not_loaded).

generate_nonce() ->
    exit(nif_library_not_loaded).

encrypt_nif(_Key, _Iv, _Plaintext) ->
    exit(nif_library_not_loaded).

encrypt(Key, Iv, Plaintext) ->
    encrypt_nif(Key, Iv, Plaintext).

decrypt_nif(_Key, _Iv, _Ciphertext) ->
    exit(nif_library_not_loaded).

decrypt(Key, Iv, Ciphertext) ->
    decrypt_nif(Key, Iv, Ciphertext).
