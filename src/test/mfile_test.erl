-module(mfile_test).
-include_lib("eunit/include/eunit.hrl").
-include("mfile.hrl").
-compile([{parse_transform, lager_transform}]).
 
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
     {"is_valid_cstr function",
      ?_test(test_is_valid_cstr())},
     {"Insert an mfilecode",
      ?_test(insert_an_mfilecode())},
     {"Get the code id as an integer",
      ?_test(test_get_code_id_as_integer())},
     {"Delete an mfilecode",
      ?_test(delete_an_mfilecode())},
     {"Insert an mfile",
      ?_test(insert_an_mfile())},
     {"Find last serial number",
      ?_test(find_last_sern())}
%     {"Description of test 3",
%      ?_test(test_function_3())}
    ].
 
test_is_valid_cstr() ->
    ?assertEqual(true, mfilelib:is_valid_cstr("test")),
    ?assertEqual(false, mfilelib:is_valid_cstr("mfilecode-1")).

insert_an_mfilecode() ->
    lager:info("Test: insert an mfilecode"),
    I = mfilelib:icode_insert("test"),
    ?assertEqual("success", I#icode.result),
    ?assertEqual("TEST", I#icode.cstr),
    ?assertNotEqual(id, I#icode.id),
    ?assertEqual(true, is_list(I#icode.id)),
    "mfilecode-" ++ CId = I#icode.id.         % format is 'mfilecode-' ++ integer

test_get_code_id_as_integer() ->
    lager:info("Test: get code id as integer"),
    I = mfilelib:get_code_id_as_integer("test"),
    ?assertEqual(true, is_integer(I)).

delete_an_mfilecode() ->
    lager:info("Test: delete an mfilecode"),
    I1 = mfilelib:icode_delete("test"),
    ?assertEqual("success", I1#icode.result),
    I2 = mfilelib:icode_delete("test"),
    ?assertEqual("Code not found", I2#icode.result).
 
insert_an_mfile() ->
    lager:info("Test: insert an mfile"),
    IC = mfilelib:icode_insert("test"),
    ?assertEqual("success", IC#icode.result),
    IF = mfilelib:ifile_insert(#ifile{
                                        cid = mfilelib:mfilecodeId_strip(IC#icode.id),
                                        cstr = IC#icode.cstr,
                                        keyw = "Test file"
                                     }),
    ?assertEqual("success", IF#ifile.result).

find_last_sern() ->
    lager:info("Test: find last serial number"),
    L1 = mfilelib:find_last_sern("testnon"),
    ?assertEqual(0, L1),
    L2 = mfilelib:find_last_sern("test"),
    ?assertEqual(1, L2).

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
