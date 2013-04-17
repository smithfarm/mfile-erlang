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
     {"test_icode_fetch",
      ?_test(test_icode_fetch())}
     ,
     {"Find last serial number of an existing code with no files in DB",
      ?_test(find_last_sern_of_existing_code_nofiles())}
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
     {"insert an mfile",
      ?_test(insert_an_mfile())}
     ,
     {"Find last serial number of an existing code with one file",
      ?_test(find_last_sern_of_existing_code_with_one_file())}
     ,
     {"fetch an mfile",
      ?_test(fetch_an_mfile())}
%     ,
%     {"Description of test 3",
%      ?_test(test_function_3())}
    ].
 
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
    ?assertEqual(0, mfiledb:find_last_code_id()).

insert_an_mfilecode() ->
    lager:info("Test: insert an mfilecode"),
    I = mfiledb:icode_insert("test"),
    ?assertEqual("success", I#icode.result),
    ?assertEqual("TEST", I#icode.cstr),
    ?assertNotEqual(id, I#icode.id),
    ?assertEqual(true, is_list(I#icode.id)),
    ?assertEqual(true, length(I#icode.id) > 0),
    test_mfilecode_validation_not_ok(I#icode.cstr, "That code is already in the database").

test_get_code_id() ->
    ?assertEqual([], mfiledb:get_code_id("testnon")),
    Val = mfiledb:get_code_id("test"),
    ?assertEqual(true, is_list(Val)),
    ?assertEqual(true, length(Val) > 0).

test_icode_fetch() ->
    R = mfiledb:icode_fetch("non-existent"),
    ?assertEqual("Code not found", R#icode.result),
    R1 = mfiledb:icode_fetch("test"),
    ?assertEqual("success", R1#icode.result).

find_last_sern_of_existing_code_nofiles() ->
    lager:info("Test: find last serial number of existing code with no files in DB"),
    CId = mfiledb:get_code_id("test"),
    L2 = mfiledb:find_last_sern(CId),
    ?assertEqual(0, L2).

find_last_code_id_non_empty_table() ->
    CId = mfiledb:find_last_code_id(),
    ?assertEqual(true, is_list(CId)),
    ?assertEqual(true, length(CId) > 0).

test_icode_has_files_when_no_files() ->
    CId = mfiledb:get_code_id("test"),
    ?assertEqual(false, mfiledb:icode_has_files(CId)),
    Cnon = mfiledb:get_code_id("testnon"),
    ?assertEqual(undefined, mfiledb:icode_has_files(Cnon)).

delete_an_mfilecode() ->
    lager:info("Test: delete an mfilecode"),
    I1 = mfiledb:icode_delete("test"),
    ?assertEqual("success", I1#icode.result),
    I2 = mfiledb:icode_delete("test"),
    ?assertEqual("Code not found", I2#icode.result).
 
find_last_sern_of_nonexist_code() ->
    lager:info("Test: find last serial number of non-existent code"),
    ?assertEqual(0, mfiledb:find_last_sern("mfilecode-55")),
    ?assertEqual(undefined, mfiledb:find_last_sern([])).

insert_an_mfile() ->
    lager:info("Test: insert an mfile"),
    IC = mfiledb:icode_insert("test"),
    lager:info("cid == ~p, cstr = ~p", [IC#icode.id, IC#icode.cstr]),
    ?assertEqual("success", IC#icode.result),
    IF = mfiledb:ifile_insert(IC#icode.cstr,
                              "Test file",
                              [] ),
    ?assertEqual("success", IF#ifile.result).

find_last_sern_of_existing_code_with_one_file() ->
    lager:info("Test: find last serial number of existing code with one file"),
    CId = mfiledb:get_code_id("test"),
    L2 = mfiledb:find_last_sern(CId),
    ?assertEqual(1, L2).

fetch_an_mfile() ->
    IF = mfiledb:ifile_fetch("test", 1),
    ?assertEqual(true, is_tuple(IF)),
    ?assertEqual("TEST", IF#ifile.cstr),
    ?assertEqual("Test file", IF#ifile.keyw).

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
