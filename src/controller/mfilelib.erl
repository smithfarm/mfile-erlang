-module(mfilelib).
-include("mfile.hrl").
-compile(export_all).

%%%============================================================================
%%% mfile - mfile library functions
%%%============================================================================

%% initializeForm/0 
%%                  %% get mfile version number and DB backend version info
%%
initializeForm() ->
   X = getMfileVerNum(),  
   % get PostgreSQL version number
   { ok, _, [ {DBS} ] } = boss_db:execute("SELECT version();"),
   % prep for sending to template (view/start.html)
   [{mfilevernum, X}, {mfiledbstatus, binary_to_list(DBS)}].


%% getMfileVerNum/0 %% extracts "vsn" value from ebin/mfile.app
%%
getMfileVerNum() -> 
   case application:get_key(mfile, vsn) of
      {ok, Result} -> Result;
      undefined    -> Result = "Undefined"
   end,
   Result.


%% icode_JSON/1 %% takes an icode instance and
%%              %% stringifies it
%%
icode_JSON(I) when is_record(I, icode) ->
   {json, [ {queryResult,   I#icode.result},
            {mfilecodeId,   I#icode.id},
            {mfilecodeDate, I#icode.dstr},
	    {mfilecodeCode, I#icode.cstr},
	    {mfilecodeDesc, I#icode.desc} ] }.


%% ifile_JSON
ifile_JSON(I) when is_record(I, ifile) ->
   {json, [ {queryResult, I#ifile.result},
            {mfileId,     I#ifile.id},
            {mfileDate,   I#ifile.dstr},
	    {mfileCodeId, I#ifile.cid},
            {mfileCode,   I#ifile.cstr},
            {mfileSern,   I#ifile.sern},
            {mfileKeyw,   I#ifile.keyw},
	    {mfileDesc,   I#ifile.desc} ] }.
  

%% is_valid_cstr/1   %% takes a string S
%%                   %% returns true or false
%%***
is_valid_cstr([H|T]) ->
   is_ASCII_letter(H)
      and
   lists:all(fun is_ASCII_letter_or_numeral/1, T)
      and 
   (length(T) < 8);
is_valid_cstr([]) ->
   false.


%% is_ASCII_letter_or_numeral %% takes a integer
%%                            %% returns true or false
%%***
is_ASCII_letter_or_numeral(X) when is_integer(X), X >= $A, X =< $Z ->
   true;
is_ASCII_letter_or_numeral(X) when is_integer(X), X >= $a, X =< $z ->
   true;
is_ASCII_letter_or_numeral(X) when is_integer(X), X >= $0, X =< $9 ->
   true;
is_ASCII_letter_or_numeral(X) when is_integer(X) ->
   false.


%% is_ASCII_letter %% takes a integer
%%                 %% returns true or false
%%***
is_ASCII_letter(X) when is_integer(X), X >= $A, X =< $Z ->
   true;
is_ASCII_letter(X) when is_integer(X), X >= $a, X =< $z ->
   true;
is_ASCII_letter(X) when is_integer(X) ->
   false.


%% uppercase_char %% takes an integer, deducts 32 if it corresponds to a lowercase letter
%%                %% returns an integer
%%***
uppercase_char(E) when is_integer(E), E >= $a, E =< $z ->
   E - 32;
uppercase_char(E) when is_integer(E) ->
   E;
uppercase_char(E) when not is_integer(E) ->
   undefined.


%% uppercase_str/1 %% takes a string, converts it to uppercase
%%                 %% using uppercase_char/1
%%
uppercase_string(E) when is_list(E), length(E) > 0 ->
   lists:map(fun uppercase_char/1, E);
uppercase_string([]) ->
   [].


%% validate_serial_number/1 %% takes a value (integer, binary, string)
%%                          %% returns 0 if it's not a natural number
%%                          %% or integer if it is
%%***
validate_serial_number(Sf) ->
   % convert "it" (whatever it is) into a list
   Sl = if
            is_list(Sf)    -> Sf;
            is_binary(Sf)  -> binary_to_list(Sf);
            is_integer(Sf) -> integer_to_list(Sf);
	    true           -> somethingElse
        end,

   % check if it's a natural number
   try 
      list_to_integer(Sl) 
   of
      Sn when (is_number(Sn)) and (Sn >= 0) -> Sn;
      Sm when (is_number(Sm)) and (Sm < 0)  -> 0
   catch
      _:_ -> 0   
   end.


%% integer_to_month %% takes an integer in the range 1-12
%%                  %% returns list containing a three-letter representation 
%%                  %% of the month
%%***
integer_to_month(M) when is_integer(M), M > 0, M < 13 ->
   case M of
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


%% timestamp_to_date_string %% takes a timestamp {{Y, M, D}, {H, M, S}}
%%                               %% returns binary <<"YYYY-MMM-DD">>
%%***
timestamp_to_date_string({{Y, M, D}, _}) when is_integer(Y), 
                                              is_integer(M),
                                              is_integer(D)  ->
   lists:append([ integer_to_list(Y), "-", 
                  integer_to_month(M), "-", 
                  integer_to_list(D) ]).


