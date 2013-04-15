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
     {"test uppercase_char function",
      ?_test(test_uppercase_char())}
     ,
     {"test uppercase_string function",
      ?_test(test_uppercase_string())}
     ,
     {"mfilecode validation",
      ?_test(test_mfilecode_validation())}
     ,
     {"find last code_id when mfilecodes table is empty",
      ?_test(find_last_code_id_empty_table())}
     ,
     {"insert an mfilecode",
      ?_test(insert_an_mfilecode())}
     ,
     {"test get_boss_code_id",
      ?_test(test_get_code_id())}
     ,
     {"find last code_id when mfilecodes table is not empty",
      ?_test(find_last_code_id_non_empty_table())}
     ,
     {"test icode_has_files function when no files in DB",
      ?_test(test_icode_has_files_when_no_files())}
     ,
     {"delete an mfilecode",
      ?_test(delete_an_mfilecode())}
     ,
     {"find last serial number of a non-existing code",
      ?_test(find_last_sern_of_nonexist_code())}
     ,
     {"test integer_to_month function",
      ?_test(test_integer_to_month())}
     ,
     {"test timestamp_to_date_string function",
      ?_test(test_timestamp_to_date_string())}
     ,
     {"insert an mfile",
      ?_test(insert_an_mfile())}
     ,
     {"Find last serial number of an existing code",
      ?_test(find_last_sern_of_existing_code())}
%     ,
%     {"Description of test 3",
%      ?_test(test_function_3())}
    ].
 
test_is_valid_cstr() ->
    ?assertEqual(true, mfilelib:is_valid_cstr("test")),
    ?assertEqual(false, mfilelib:is_valid_cstr("1test")),
    ?assertEqual(true, mfilelib:is_valid_cstr("test1")),
    ?assertEqual(true, mfilelib:is_valid_cstr("TEST")),
    ?assertEqual(false, mfilelib:is_valid_cstr("1TEST")),
    ?assertEqual(true, mfilelib:is_valid_cstr("TEST1")),
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

test_uppercase_char() ->
    ?assertEqual($A, mfilelib:uppercase_char($A)),
    ?assertEqual($Z, mfilelib:uppercase_char($Z)),
    ?assertEqual($A, mfilelib:uppercase_char($a)),
    ?assertEqual($Z, mfilelib:uppercase_char($Z)),
    ?assertEqual($0, mfilelib:uppercase_char($0)).

test_uppercase_string() ->
    ?assertEqual("TEST", mfilelib:uppercase_string("test")),
    ?assertEqual("TEST", mfilelib:uppercase_string("TEst")),
    ?assertEqual("T1", mfilelib:uppercase_string("t1")),
    ?assertEqual("T1", mfilelib:uppercase_string("T1")).

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
    test_mfilecode_validation_not_ok("123", "Malformed code"),
    test_mfilecode_validation_not_ok("A**", "Malformed code"),
    test_mfilecode_validation_ok("hippo").

find_last_code_id_empty_table() ->
    ?assertEqual(0, mfilelib:find_last_code_id()).

insert_an_mfilecode() ->
    lager:info("Test: insert an mfilecode"),
    I = mfilelib:icode_insert("test"),
    ?assertEqual("success", I#icode.result),
    ?assertEqual("TEST", I#icode.cstr),
    ?assertNotEqual(id, I#icode.id),
    ?assertEqual(true, is_list(I#icode.id)),
    ?assertEqual(true, length(I#icode.id) > 0),
    test_mfilecode_validation_not_ok(I#icode.cstr, "That code is already in the database").

test_get_code_id() ->
    ?assertEqual([], mfilelib:get_code_id("testnon")),
    Val = mfilelib:get_code_id("test"),
    ?assertEqual(true, is_list(Val)),
    ?assertEqual(true, length(Val) > 0).

find_last_code_id_non_empty_table() ->
    CId = mfilelib:find_last_code_id(),
    ?assertEqual(true, is_list(CId)),
    ?assertEqual(true, length(CId) > 0).

test_icode_has_files_when_no_files() ->
    CId = mfilelib:get_code_id("test"),
    ?assertEqual(false, mfilelib:icode_has_files(CId)),
    Cnon = mfilelib:get_code_id("testnon"),
    ?assertEqual(undefined, mfilelib:icode_has_files(Cnon)).

delete_an_mfilecode() ->
    lager:info("Test: delete an mfilecode"),
    I1 = mfilelib:icode_delete("test"),
    ?assertEqual("success", I1#icode.result),
    I2 = mfilelib:icode_delete("test"),
    ?assertEqual("Code not found", I2#icode.result).
 
find_last_sern_of_nonexist_code() ->
    lager:info("Test: find last serial number of non-existent code"),
    ?assertEqual(0, mfilelib:find_last_sern("testnon")),
    ?assertEqual(undefined, mfilelib:find_last_sern([])).

test_integer_to_month() ->
    ?assertError(function_clause, mfilelib:integer_to_month(0)),
    ?assertError(function_clause, mfilelib:integer_to_month(13)),
    ?assertError(function_clause, mfilelib:integer_to_month("123")),
    ?assertEqual("JUL", mfilelib:integer_to_month(7)).

test_timestamp_to_date_string() ->
    ?assertError(function_clause, mfilelib:timestamp_to_date_string(0)),
    ?assertEqual("2013-JUL-1", mfilelib:timestamp_to_date_string(
                                  {{2013, 7, 1}, {0, 0, 0}})).

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

find_last_sern_of_existing_code() ->
    lager:info("Test: find last serial number of existing code"),
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
