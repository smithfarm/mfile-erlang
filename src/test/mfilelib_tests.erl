-module(mfilelib_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).
 
%%%============================================================================
%%% mfilelib_test - unit tests for mfilelib module
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
     {"test uppercase_char function",
      ?_test(test_uppercase_char())}
     ,
     {"test uppercase_string function",
      ?_test(test_uppercase_string())}
     ,
     {"validate_serial_number function",
      ?_test(test_validate_serial_number())}
     ,
     {"test integer_to_month function",
      ?_test(test_integer_to_month())}
     ,
     {"test timestamp_to_date_string function",
      ?_test(test_timestamp_to_date_string())}
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

test_integer_to_month() ->
    ?assertError(function_clause, mfilelib:integer_to_month(0)),
    ?assertError(function_clause, mfilelib:integer_to_month(13)),
    ?assertError(function_clause, mfilelib:integer_to_month("123")),
    ?assertEqual("JUL", mfilelib:integer_to_month(7)).

test_timestamp_to_date_string() ->
    ?assertError(function_clause, mfilelib:timestamp_to_date_string(0)),
    ?assertEqual("2013-JUL-1", mfilelib:timestamp_to_date_string(
                                  {{2013, 7, 1}, {0, 0, 0}})).

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
