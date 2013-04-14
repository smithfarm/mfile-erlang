-module(mfile_test).
-include_lib("eunit/include/eunit.hrl").
-include("mfile.hrl").
 
%%%============================================================================
%%% API
%%%============================================================================
 
suite_test_()->
    Suite =
    {foreach, local,
      fun setup/0,
      tests()
     },
    Suite.
 
tests() ->
    [       
     {"Insert an mfilecode",
      ?_test(insert_an_mfilecode())},
     {"Delete an mfilecode",
      ?_test(delete_an_mfilecode())}
%     {"Description of test 3",
%      ?_test(test_function_3())}
    ].
 
insert_an_mfilecode() ->
    I = mfilelib:icode_insert("test"),
    ?assertEqual("success", I#icode.result),
    ?assertEqual("TEST", I#icode.cstr),
    ?assertNotEqual(id, I#icode.id).
 
delete_an_mfilecode() ->
%    I0 = mfilelib:icode_insert("dtst"),
%    ?assertEqual("success", I0#icode.result),
    I1 = mfilelib:icode_delete("test"),
    ?assertEqual("success", I1#icode.result),
    I2 = mfilelib:icode_delete("test"),
    ?assertEqual("Code not found", I2#icode.result).
 
%test_function_3() ->
%     
 
%% ===================================================================
%% Internal functions
%% ===================================================================
 
%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------
setup()->
    ok.
