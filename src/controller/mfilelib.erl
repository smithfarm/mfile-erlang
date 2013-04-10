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
            is_binary(Sf)  -> binary_to_list(Sf);
            is_integer(Sf) -> integer_to_list(Sf);
            is_list(Sf)    -> Sf;
	    true           -> somethingElse
        end,

   % check if it's an integer
   S = case catch list_to_integer(Sl) of
          Sn when ((is_number(Sn)) and (Sn > 0)) -> R = "success",
                                                    Sn; 
          _ -> R = "Invalid serial number",
               0   
       end,
   {{result, R}, {sern, S}}.

validate_codestr(Cf) ->
   I = case fetch_code(Cf) of
          {"success", T, _, C, _} -> R = "success",
	                             T;
          _                       -> R = lists:append(["Code '", Cf, "' not found"]),
	                             C = "",
	                             0
       end,
   {{result, R}, {codeid, I}, {codestr, C}}.

validate_codestr_and_sern(Cf, Sf) ->
   {Rs, S} = validate_serial_number(Sf),
   if
      S =:= {sern, 0} -> {Rs, {codeid, 0}, {codestr, Cf}, S};  % invalid serial number
      S =/= {sern, 0} -> {Rc, I, C} = validate_codestr(Cf),
                         {Rc, I, C, S}
   end.

% given a code string, convert it to all upper-case and look up its vitals
% returns {QUERY_RESULT_STRING, CODE_ID, CREATED_AT, CODE_STR, CODE_DESC}
fetch_code(C) ->
   case boss_db:find(mfilecode, [{code_str, 'equals', lists:map(fun uppercase_it/1, C)}]) of
      [{mfilecode,CId,CTime,CStr,CDesc}] -> { "success", 
                                              CId,
                                              mfilelib:timestamp_to_binary_date_only(CTime),
					      CStr,
					      CDesc };
      []                                 -> { lists:append(["Code '", C, "' not found"]),
                                              0,
                                              "",
					      "",
					      "" }
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
    %io:format("is_a_letter: received element ~p.~n", [X]),
    UpperAndLowerCaseLetters = lists:append(lists:seq(65,90), lists:seq(97,122)),
    lists:member(X, UpperAndLowerCaseLetters).

validate_mfilecode(Arg) when is_list(Arg) ->
    lists:all(fun mfilelib:is_an_ASCII_letter/1, Arg);
validate_mfilecode(_) ->
    {error, "Argument must be a list."}.
