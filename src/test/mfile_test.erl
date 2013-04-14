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
      ?_test(test_is_valid_cstr())}
     ,
     {"validate_serial_number function",
      ?_test(test_validate_serial_number())}
     ,
     {"mfilecode validation",
      ?_test(test_mfilecode_validation())}
     ,
     {"Insert an mfilecode",
      ?_test(insert_an_mfilecode())}
     ,
     {"Get the code id as an integer",
      ?_test(test_get_code_id_as_integer())}
     ,
     {"Delete an mfilecode",
      ?_test(delete_an_mfilecode())}
     ,
     {"Insert an mfile",
      ?_test(insert_an_mfile())}
     ,
     {"Find last serial number",
      ?_test(find_last_sern())}
%     ,
%     {"Description of test 3",
%      ?_test(test_function_3())}
    ].
 
test_is_valid_cstr() ->
    ?assertEqual(true, mfilelib:is_valid_cstr("test")),
    ?assertEqual(false, mfilelib:is_valid_cstr("mfilecode-1")).

test_validate_serial_number() ->
    ?assertEqual(123, mfilelib:validate_serial_number(123)),
    ?assertEqual(123, mfilelib:validate_serial_number("123")),
    ?assertEqual(123, mfilelib:validate_serial_number(<<"123">>)),
    ?assertEqual(0, mfilelib:validate_serial_number(a)),
    ?assertEqual(0, mfilelib:validate_serial_number(-1)),
    ?assertEqual(0, mfilelib:validate_serial_number(0)),
    ?assertEqual(0, mfilelib:validate_serial_number(3.145)),
    ?assertEqual(0, mfilelib:validate_serial_number([])),
    ?assertEqual(0, mfilelib:validate_serial_number({123})).

test_mfilecode_validation_ok(CStr) ->
    BossRec = boss_record:new(mfilecode, [ { code_id, 1 },
                                           { code_str, CStr } ] ),
    R = BossRec:validate(),
    ?assertEqual(ok, R).

test_mfilecode_validation_not_ok(CStr, ErrorMsg) ->
    BossRec = boss_record:new(mfilecode, [ { code_id, 1 },
                                           { code_str, CStr } ] ),
    {error, [ErrorMsg|_]} = BossRec:validate().

test_mfilecode_validation() ->
    test_mfilecode_validation_not_ok(abc, "Invalid data type for code_str"),
    test_mfilecode_validation_not_ok([], "Code is empty"),
    test_mfilecode_validation_not_ok("NineChars", "Code string too long (max. 8 characters)"),
    test_mfilecode_validation_not_ok("123", "Upper and lower case ASCII characters only"),
    test_mfilecode_validation_ok("hippo").

insert_an_mfilecode() ->
    lager:info("Test: insert an mfilecode"),
    I = mfilelib:icode_insert("test"),
    ?assertEqual("success", I#icode.result),
    ?assertEqual("TEST", I#icode.cstr),
    ?assertNotEqual(id, I#icode.id),
    ?assertEqual(true, is_integer(I#icode.id)),
    I#icode.id > 0,
    test_mfilecode_validation_not_ok(I#icode.cstr, "That code is already in the database").


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
                                        cid = IC#icode.id,
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
