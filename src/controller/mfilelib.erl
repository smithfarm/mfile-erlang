-module(mfilelib).
-compile(export_all).

% 
% Function to validate serial number entered by user into the form:
% test if it's a number and, if it is, convert it into an integer
% Returns tuple {{result, RESULT_STRING}, {sern, SERIAL_NUMBER}}
%
validate_serial_number(Sf) ->
   % convert "it" (whatever it is) into a list
   Sl = if
            is_list(Sf)    -> Sf;
            is_binary(Sf)  -> binary_to_list(Sf);
            is_integer(Sf) -> integer_to_list(Sf);
	    true           -> somethingElse
        end,

   % check if it's a natural number
   S = case catch list_to_integer(Sl) of
          Sn when ((is_number(Sn)) and (Sn > 0)) -> R = "success",
                                                    Sn; 
          _ -> R = "Invalid serial number",
               0   
       end,
   {{result, R}, {sern, S}}.

validate_codestr(Cf) ->
   lager:info("Entering mfilelib:validate_codestr/1"),
   I = case fetch_code(Cf) of
          {R, T, _, C, _} when R =:= "success" -> T;
          {R, T, _, C, _}                      -> T
       end,
   {{result, R}, {codeid, I}, {codestr, C}}.

validate_codestr_and_sern(Cf, Sf) ->
   {Rs, S} = validate_serial_number(Sf),
   if
      S =:= {sern, 0} -> {Rs, {codeid, 0}, {codestr, Cf}, S};  % invalid serial number
      S =/= {sern, 0} -> {Rc, I, C} = validate_codestr(Cf),
                         {Rc, I, C, S}
   end.

%
% the Boss DB model returns the Code ID in the format "mfilecode-" ++ SOME_INTEGER, 
% (e.g. "mfilecode-1"), but to be useful to us we need to strip off the "mfilecode-"
% part and leave just the integer
%
mfilecodeId_strip([$m,$f,$i,$l,$e,$c,$o,$d,$e,$-|T]) ->  
   list_to_integer(T).  % isn't Erlang amazing?

% given a code string, convert it to all upper-case and look up its vitals
% returns {QUERY_RESULT_STRING, CODE_ID, CREATED_AT, CODE_STR, CODE_DESC}
fetch_code(C) ->
   % use find_first just to be on the safe side
   R = boss_db:find_first(mfilecode, [{code_str, 'equals', lists:map(fun uppercase_it/1, C)}]),
   lager:info("boss_db:find_first(mfilecode) returned: ~p", [R]),
   case R of
      {mfilecode,CId,CTime,CStr,CDesc} -> { "success", 
                                            mfilecodeId_strip(CId),
                                            mfilelib:timestamp_to_binary_date_only(CTime),
					    CStr,
					    CDesc };
      undefined                        -> { lists:append(["Code ", C, " not found"]),
                                            0, [], [], [] };
      _                                -> { "internal error",
                                            0, [], [], [] }

   end.


getMfileVerNum() -> 
   case application:get_key(mfile, vsn) of
      {ok, Result} -> Result;
      undefined    -> Result = "Undefined"
   end,
   Result.


uppercase_it(E) ->
   case lists:member(E, lists:seq(97,122)) of
      true -> E - 32;
      false -> E
   end.


integer_to_month(MonInt) ->
   case MonInt of
      1 -> "JAN";
      2 -> "FEB";
      3 -> "MAR";
      4 -> "APR";
      5 -> "MAY";
      6 -> "JUN";
      7 -> "JUL";
      8 -> "AUG";
      9 -> "SEP";
      10 -> "OCT";
      11 -> "NOV";
      12 -> "DEC"
   end.


timestamp_to_binary_date_only(Timest) ->
   {{Y, M, D},{_,_,_}} = Timest,
   list_to_binary( [ integer_to_list(Y), "-", 
                     integer_to_month(M), "-", 
                     integer_to_list(D) ] ).


is_an_ASCII_letter(X) ->
    Uppers = lists:seq($A, $Z),
    Lowers = lists:seq($a, $z),
    lists:member(X, Uppers) or lists:member(X, Lowers).


%
% to be fit for insertion into the database, an mfilecode must satisfy several conditions:
% 1. must be a list
% 2. must have more than zero members
% 3. must have less than nine members
% 4. must consist of upper and lower case ASCII characters only
% 5. must not already exist in codes table of database
%
mfilecode_ok_for_insert(Arg, 5) -> 
   case fetch_code(Arg) of
      {"success", _, _, _, _} -> "Code already exists in the database";
      _ -> yes
   end;
mfilecode_ok_for_insert(Arg, 4) -> 
   case lists:all(fun mfilelib:is_an_ASCII_letter/1, Arg) of 
      true -> mfilecode_ok_for_insert(Arg, 5);
      _ -> "Upper and lower case ASCII characters only"
   end;
mfilecode_ok_for_insert(Arg, 3) -> 
   if
      length(Arg) < 9 -> mfilecode_ok_for_insert(Arg, 4);
      true -> "Code string too long (max. 8 characters)"
   end;
mfilecode_ok_for_insert(Arg, 2) -> 
   if
      length(Arg) > 0 -> mfilecode_ok_for_insert(Arg, 3);
      true -> "Code string is empty"
   end;
mfilecode_ok_for_insert(Arg, 1) -> 
   if
      is_list(Arg) -> mfilecode_ok_for_insert(Arg, 2);
      true -> "Not a list"
   end.

mfilecode_ok_for_insert(Arg) ->
   Result = mfilecode_ok_for_insert(Arg, 1),
   case Result of
      yes -> yes;
      ErrorMessage -> ErrorMessage
   end.
