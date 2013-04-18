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
     {"mfilecode validation of invalid strings",
      ?_test(test_mfilecode_validation_not_ok())}
     ,
     {"mfilecode validation of a valid string",
      ?_test(test_mfilecode_validation_ok())}
     ,
     {"insert an mfilecode",
      ?_test(insert_an_mfilecode())}
     ,
     {"attempt to insert malformed mfilecodes",
      ?_test(insert_an_mfilecode_not_ok())}
     , 
     {"test get_boss_code_id",
      ?_test(test_get_code_id())}
     ,
     {"test icode_exists_cstr",
      ?_test(test_icode_exists_cstr())}
     ,
     {"test icode_exists_cstr",
      ?_test(test_icode_exists_cid())}
     ,
     {"test_icode_fetch",
      ?_test(test_icode_fetch())}
     ,
     {"Find last serial number of an existing code with no files in DB",
      ?_test(find_last_sern_of_existing_code_nofiles())}
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
     {"attempt to insert invalid mfile",
      ?_test(attempt_insert_invalid_mfile())}
     ,
     {"test get_file_id",
      ?_test(test_get_file_id())}
     ,
     {"Find last serial number of an existing code with one file",
      ?_test(find_last_sern_of_existing_code_with_one_file())}
     ,
     {"fetch an mfile",
      ?_test(fetch_an_mfile())}
     ,
     {"update an mfile",
      ?_test(test_ifile_update())}
%     ,
%     {"Description of test 3",
%      ?_test(test_function_3())}
    ].
 
test_mfilecode_validation_not_ok(CStr, DesiredErr) ->
    BossRec = boss_record:new(mfilecode, [ { code_id, 1 },
                                           { code_str, CStr } ] ),
    {error, [ReceivedErr|_]} = BossRec:validate(),
    ?assertEqual(DesiredErr, ReceivedErr).

test_mfilecode_validation_not_ok() ->
    lager:info("Test: try to validate several invalid mfile Code strings"),
    test_mfilecode_validation_not_ok(abc, "Invalid data type for code_str"),
    test_mfilecode_validation_not_ok([], "Code is empty"),
    test_mfilecode_validation_not_ok("NineChars", "Code string too long (max. 8 characters)"),
    test_mfilecode_validation_not_ok("123", "Malformed code"),
    test_mfilecode_validation_not_ok("A**", "Malformed code").

test_mfilecode_validation_ok(CStr) ->
    BossRec = boss_record:new(mfilecode, [ { code_id, 1 },
                                           { code_str, CStr } ] ),
    R = BossRec:validate(),
    ?assertEqual(ok, R).

test_mfilecode_validation_ok() ->
    lager:info("Test: validate a valid mfile Code string"),
    test_mfilecode_validation_ok("hippo").

insert_an_mfilecode() ->
    lager:info("Test: insert an mfilecode"),
    I = mfiledb:icode_insert("test"),
    ?assertEqual("success", I#icode.result),
    ?assertEqual("TEST", I#icode.cstr),
    ?assertNotEqual(id, I#icode.id),
    ?assertEqual(true, is_list(I#icode.id)),
    ?assertEqual(true, length(I#icode.id) > 0),
    test_mfilecode_validation_not_ok(I#icode.cstr, "That code is already in the database").

insert_an_mfilecode_not_ok() ->
    lager:info("Test: attempt to insert some malformed mfilecodes"),
    I1 = mfiledb:icode_insert("And now is the time for all good men to come in and rock"),
    ?assertEqual("Malformed code string", I1#icode.result),
    I1 = mfiledb:icode_insert("mfilecode-1"),
    ?assertEqual("Malformed code string", I1#icode.result),
    I1 = mfiledb:icode_insert([]),
    ?assertEqual("Malformed code string", I1#icode.result),
    ?assertError(function_clause, mfiledb:icode_insert(123)),
    ?assertError(function_clause, mfiledb:icode_insert(abc)).

test_get_code_id() ->
    lager:info("Test: look up code IDs by code strings (non-existent and existing)"),
    ?assertEqual([], mfiledb:get_code_id("testnon")),
    Val = mfiledb:get_code_id("test"),
    lager:info("mfiledb:get_code_id() returned ~p", [Val]),
    ?assertEqual(true, is_list(Val)),
    ?assertEqual(true, length(Val) > 0).

test_icode_exists_cstr() ->
    lager:info("Test: whether a given code string exists"),
    ?assertEqual(false, mfiledb:icode_exists_cstr("testnon")),
    ?assertEqual(true, mfiledb:icode_exists_cstr("test")).

test_icode_exists_cid() ->
    lager:info("Test: whether a given code ID exists"),
    ?assertEqual(false, mfiledb:icode_exists_cid("mfilecode-55")),
    CId = mfiledb:get_code_id("test"),
    ?assertEqual(true, mfiledb:icode_exists_cid(CId)).

test_icode_fetch() ->
    lager:info("Test: fetch entire code record by code string"),
    R = mfiledb:icode_fetch("non-existent"),
    ?assertEqual("Code not found", R#icode.result),
    R1 = mfiledb:icode_fetch("test"),
    ?assertEqual("success", R1#icode.result).

find_last_sern_of_existing_code_nofiles() ->
    lager:info("Test: find last serial number of existing code with no files in DB"),
    CId = mfiledb:get_code_id("test"),
    L2 = mfiledb:find_last_sern(CId),
    ?assertEqual(0, L2).

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
    lager:info("IC#icode.result == ~p", [IC#icode.result]),
    ?assertEqual("mfilecode-2", IC#icode.id),
    ?assertEqual("TEST", IC#icode.cstr),
    ?assertEqual("success", IC#icode.result),
    IF = mfiledb:ifile_insert(IC#icode.cstr,
                              "Test file",
                              [] ),
    ?assertEqual("success", IF#ifile.result).

attempt_insert_invalid_mfile() ->
    lager:info("Test: attempt to insert an invalid mfile"),
    IF = mfiledb:ifile_insert("HorbleyDorbley",
                              "Test file",
                              [] ),
    ?assertEqual("Attempt to insert file with invalid Code", IF#ifile.result).

test_get_file_id() ->
    %need a function that checks if a string is an mfile/mfilecode ID    
    %something that splits the string into parts before and after the "-"
    %and checks if the second part is an integer, etc.
    FId = mfiledb:get_file_id("test", 1),
    lager:info("test_get_file_id() got FId ~p", [FId]),
    [Type, TableId] = re:split(FId, "-", 
                               [{return, list}, {parts, 2}]),
    ?assertEqual("mfile", Type),
    I = list_to_integer(TableId),
    ?assertEqual(true, is_integer(I)),
    F2 = mfiledb:get_file_id("arbleberryness", 3543),
    ?assertEqual([], F2).

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

test_ifile_update() ->
    R1 = mfiledb:ifile_update("TEST", 1, "Updated keywords", "Updated description"),
    ?assertEqual("success", R1#ifile.result),
    R2 = mfiledb:ifile_fetch("test", 1), 
    ?assertEqual(true, is_tuple(R2)),
    ?assertEqual("TEST", R2#ifile.cstr),
    ?assertEqual("Updated keywords", R2#ifile.keyw),
    ?assertEqual("Updated description", R2#ifile.desc).


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


