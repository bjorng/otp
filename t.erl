-module(t).
-compile([export_all,nowarn_export_all]).

t() ->
    (catch crypto:stop()),
    code:delete(crypto),
    code:purge(crypto),
    Key0 = "ablurf123BX#$;3",
    Bin0 = erlang:md5(<<"whatever">>),
    {Key,IVec,BlockSize} = make_crypto_key(Key0),
    crypto:crypto_one_time(des_ede3_cbc, Key, IVec, Bin0, true).

make_crypto_key(String) ->
    <<K1:8/binary,K2:8/binary>> = First = erlang:md5(String),
    <<K3:8/binary,IVec:8/binary>> = erlang:md5([First|lists:reverse(String)]),
    {[K1,K2,K3],IVec,8}.
