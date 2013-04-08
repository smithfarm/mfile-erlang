-module(mfilelib).
-compile(export_all).

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
