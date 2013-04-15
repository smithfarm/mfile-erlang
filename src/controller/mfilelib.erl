-module(mfilelib).
-compile(export_all).
-include("mfile.hrl").


%% initializeForm/0 
%%                  %% get mfile version number and DB backend version info
%%***
initializeForm() ->
   X = getMfileVerNum(),  
   % get PostgreSQL version number
   { ok, _, [ {DBS} ] } = boss_db:execute("SELECT version();"),
   % prep for sending to template (view/start.html)
   [{mfilevernum, X}, {mfiledbstatus, binary_to_list(DBS)}].


%% getMfileVerNum/0 %% extracts "vsn" value from ebin/mfile.app
%%***
getMfileVerNum() -> 
   case application:get_key(mfile, vsn) of
      {ok, Result} -> Result;
      undefined    -> Result = "Undefined"
   end,
   Result.


%% is_ASCII_letter_or_numeral %% takes a integer
%%                            %% returns true or false
%%
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
%%
is_ASCII_letter(X) when is_integer(X), X >= $A, X =< $Z ->
   true;
is_ASCII_letter(X) when is_integer(X), X >= $a, X =< $z ->
   true;
is_ASCII_letter(X) when is_integer(X) ->
   false.


%% is_valid_cstr/1   %% takes a string S
%%                   %% returns true or false
%%***
is_valid_cstr([H|T]) ->
   is_ASCII_letter(H) and lists:all(fun is_ASCII_letter_or_numeral/1, T);
is_valid_cstr([]) ->
   false.


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


%% find_last_code_id/0 %% 
%%                     %% returns an integer: either the last
%%                     %% code_id/CodeID or 0 if table empty
%%***
find_last_code_id() ->
   R = boss_db:find_last(mfilecode, []),
   lager:info("boss_db:find_last(mfilecode) returned: ~p", [R]),
   ReturnVal = case R of
                  {mfilecode,_,_,S,_,_} -> S;
                  undefined ->             0
               end,
   true = is_integer(ReturnVal),
   lager:info("find_last_code_id(): returning ~p", [ReturnVal]),
   ReturnVal.


%% uppercase_it %% takes an integer, deducts 32 if it corresponds to a lowercase letter
%%              %% returns an integer
%%***
uppercase_it(E) when E >= $0, E =< $9 ->
   E;
uppercase_it(E) when E >= $A, E =< $Z ->
   E;
uppercase_it(E) when E >= $a, E =< $z ->
   E - 32.


%% icode_insert/1 %% takes a code string
%%                %% returns a populated icode record
%%***
icode_insert(CStr) when is_list (CStr) ->
   true = is_valid_cstr(CStr),
   MfilecodeRec = mfilecode:new( id,
                                 calendar:now_to_datetime(erlang:now()),
                                 find_last_code_id() + 1,
				 lists:map(fun mfilelib:uppercase_it/1, CStr),
				 [] ),   % code_desc field is currently unused
   R = MfilecodeRec:save(),  
   %lager:info("MfilecodeRec:save() returned: ~p", [R]),
   icode_insert(R);
icode_insert( {ok, BossRec} ) ->
   %lager:info("MfilecodeRec:id() == ~p", [CId]),
   #icode{result = "success", 
          id = BossRec:code_id(),
   	  dstr = mfilelib:timestamp_to_date_string(BossRec:created_at()),
	  cstr = BossRec:code_str()};
icode_insert( {error, [FirstErrMesg|_]} ) -> 
   #icode{result = FirstErrMesg}.


%% get_code_id_as_integer/1    %% takes a string (CStr)
%%                             %% returns an integer (CId)
%%***
get_code_id_as_integer(CStr) when is_list(CStr) ->
   R = icode_fetch(CStr),
   ReturnVal = R#icode.id,
   true = is_integer(ReturnVal),
   ReturnVal.


%% icode_exists %% takes a CId (integer) or CStr (list)
%%              %% returns true or false
%%
icode_exists(CId) when is_integer(CId), CId > 0 -> 
   lager:info("Preparing to fetch Code ID ~p", [CId]),
   I = icode_fetch(CId),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> true;
      _ -> false
   end;
icode_exists(CStr) when is_list(CStr), CStr =/= [] -> 
   lager:info("Preparing to fetch Code ~p", [CStr]),
   I = icode_fetch(CStr),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> true;
      _ -> false
   end;
icode_exists(_) ->
   false.


%% get_boss_code_id/1 ** takes string (CStr)
%%                    ** returns a string something like "mfilecode-8"
%%                    ** if found, but don't rely on it having that format
%%                    ** if not found, returns []
%%
get_boss_code_id(CStr) when is_list(CStr) ->
   true = is_valid_cstr(CStr),
   R = boss_db:find_first(mfilecode, [{code_str, 
                                       'equals', 
				       lists:map(fun uppercase_it/1, CStr)} ]),
   lager:info("get_boss_code_id(): boss_db:find_first(mfilecode) returned ~p", [R]),
   case R of
      undefined -> [];
      _ ->         R:id()
   end.


%% icode_delete/1 %% takes a string (file code)
%%                %% returns a populated icode record
%%***
icode_delete(CStr) when is_list(CStr) ->
   true = is_valid_cstr(CStr),
   BoolVal = icode_exists(CStr),
   case BoolVal of
      true ->
         CId = get_code_id_as_integer(CStr),
         lager:info("icode_delete/1: Found mfilecode: ~p", [CId]),
         case icode_has_files(CId) of
	    true ->
	       #icode{result = "Files exist for this code: can't delete"};
	    false ->
               CodeId = get_boss_code_id(CStr),
               lager:info("icode_delete/1: The record has Boss ID: ~p", [CodeId]),
               lager:info("icode_delete/1: About to delete the file code with ID ~p", [CodeId]),
               case boss_db:delete(CodeId) of
                  ok -> 
                     #icode{result = "success"}; 
                  {error, DelErrMesg} ->
                     #icode{result = DelErrMesg}
	       end
	 end;
      false ->
         #icode{result = "Code not found"}
   end.


%% find_last_sern/1 %% takes a string (CStr)
%%                  %% returns an integer (Sern)
%%
%% Finds the current last serial number for a given CodeStr
%% Returns 0 if the CodeId doesn't exist
%%***
find_last_sern(CStr) when is_list(CStr) ->
   true = is_valid_cstr(CStr),
   CId = get_code_id_as_integer(CStr),
   lager:info("find_last_sern(): CId == ~p", [CId]),
   R = boss_db:find_last(mfile, [{code_id, 'equals', CId}]),
   lager:info("boss_db:find_last(mfile) returned: ~p", [R]),
   ReturnVal = case R of
                  {mfile,_,_,_,S,_,_} -> S;
                  undefined ->           0
               end,
   true = is_integer(ReturnVal),
   lager:info("find_last_sern(): returning ~p", [ReturnVal]),
   ReturnVal.


%% integer_to_month %% takes an integer in the range 1-12
%%                  %% returns list containing a three-letter representation 
%%                  %% of the month
%%***
integer_to_month(M) when is_integer(M) and (M > 0) and (M < 13) ->
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
timestamp_to_date_string({{Y, M, D}, _}) ->
   lists:append([ integer_to_list(Y), "-", 
                  integer_to_month(M), "-", 
                  integer_to_list(D) ]).


%% ifile_insert %% takes an ifile record populated with values (cid, cstr, keyw, file_desc)
%%              %% inserts new file and returns completed populated ifile record
%%***              
ifile_insert(I) when is_record(I, ifile) ->
   true = is_integer(I#ifile.cid),
   true = is_valid_cstr(I#ifile.cstr),
   MfileRec = boss_record:new(mfile, 
         [ 
            {created_at, calendar:now_to_datetime(erlang:now())}, 
            {code_id,    I#ifile.cid},
            {sern,       find_last_sern(I#ifile.cstr) + 1},  % not atomic, unfortunately!
            {keyw,       I#ifile.keyw}, 
            {file_desc,  I#ifile.desc} 
         ] ),
   lager:info("boss_record:new(mfile) returned: ~p", [MfileRec]),
   BossRec = MfileRec:save(),
   lager:info("MfileRec:save() returned: ~p", [BossRec]),
   case BossRec of   %% N.B.: Does not update id field!!
      {ok, {mfile, BossId, _, _, _, _, _}} ->
	  #ifile{ result = "success",
                  id =     BossId,
                  dstr =   mfilelib:timestamp_to_date_string(MfileRec:created_at()),
		  cid =    MfileRec:code_id(),
                  cstr =   I#ifile.cstr,
                  sern =   MfileRec:sern(),
                  keyw =   MfileRec:keyw(), 
                  desc =   MfileRec:file_desc() };
      {error, SaveErrMesg} -> 
          #ifile{result = SaveErrMesg}
   end.


%% icode_fetch/1 %% Takes either a CId or a CStr
%%               %% Fetches it from the DB and returns a populated icode record
%%
icode_fetch(CId) when is_number(CId) ->
   icode_fetch( 
      boss_db:find_first(mfilecode, [{id, 
                                     'equals', 
                                     lists:append("mfilecode-", integer_to_list(CId))} ])
   );
icode_fetch(CStr) when is_list(CStr) ->
   icode_fetch( 
      case is_valid_cstr(CStr) of
          true -> 
             boss_db:find_first(mfilecode, [{code_str, 
                                            'equals', 
                                            lists:map(fun uppercase_it/1, CStr)} ]);
          false ->
             undefined
       end
    );
icode_fetch(BossRec) ->
   lager:info("icode_fetch(BossRec) called with argument: ~p", [BossRec]),
   case BossRec of
      undefined -> 
         #icode{ result = "Code not found",
                 id     = 0 };
      _ ->
         #icode{ result = "success", 
                 id     = BossRec:code_id(),
                 dstr   = timestamp_to_date_string(BossRec:created_at()),
		 cstr   = BossRec:code_str(),
		 desc   = BossRec:code_desc() }
   end.


%% icode_has_files/1 %% takes an integer (CId), e.g. 8, not "mfilecode-8"
%%                   %% returns true or false
%%
icode_has_files(CId) when is_integer(CId) ->
   lager:info("Looking up files that have code ID: ~p", [CId]),
   Count = boss_db:count(mfile, [{code_id, 'equals', CId}]),
   lager:info("Number of files found: ~p", [Count]),
   case Count of
      0 -> false;
      _ -> true
   end.


%% icode_JSON/1 %% takes an icode instance and
%%              %% stringifies it
%%
icode_JSON(I) when is_record(I, icode) ->
   {json, [ {queryResult,   I#icode.result},
            {mfilecodeId,   I#icode.id},
            {mfilecodeDate, I#icode.dstr},
	    {mfilecodeCode, I#icode.cstr},
	    {mfilecodeDesc, I#icode.desc} ] }.


%% ifile_exists/2 %% takes a string (CStr) and an integer (Sern)
%%                %% returns true or false
%%
ifile_exists(CStr, Sern) when is_list(CStr) and is_integer(Sern) -> 
   true = is_valid_cstr(CStr),
   I = ifile_fetch(CStr, Sern),
   case I#ifile.result of
      "success" -> true;
      _         -> false
   end.


%% ifile_fetch/2 %% takes a CStr and Sern
%%               %% returns a populated ifile record
%%
ifile_fetch(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   ICode = icode_fetch(CStr),
   RetVal = 
      case ICode#icode.result of 
         "success" -> ifile_fetch(#ifile{cid =  ICode#icode.id,
                                         cstr = ICode#icode.cstr,
                                         sern = Sern});
         R ->         #ifile{ result = R }
   end.


%% ifile_fetch/1 %% takes an ifile record populated with cid, cstr, and sern
%%               %% returns a populated ifile record
ifile_fetch(I) when is_record(I, ifile) ->
   % there shouldn't be more than one, but we use find_first anyway
   R = boss_db:find_first(mfile, 
      [
         {code_id, 'equals', I#ifile.cid}, 
         {sern, 'equals', I#ifile.sern}
      ] ),
   lager:info("boss_db:find_first(mfile) returned: ~p", [R]),
   case R of
      {mfile,V1,V2,_,V4,V5,V6} -> 
         #ifile{ result = "success",
                 id     = V1,
                 dstr   = mfilelib:timestamp_to_date_string(V2),
                 cid    = I#ifile.cid,
                 cstr   = I#ifile.cstr,
                 sern   = V4,
                 keyw   = V5, 
                 desc   = V6};
      undefined -> 
         UndefRes = "Code/Serial Number combination not found",
         lager:info("Returning error ~p", [UndefRes]),
         #ifile{ result = UndefRes };
      _ -> 
         #ifile{ result = "Internal error on fetch (see log for details)" }
   end.


%% ifile_delete/2 %% takes a string (CStr) and an integer (Sern)
%%                %% returns a populated ifile record
%%
ifile_delete(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   true = is_valid_cstr(CStr),
   lager:info("Entering ifile_delete with arguments: ~p, ~p", [CStr, Sern]),
   case icode_exists(CStr) of
      false ->
         #ifile{result = "No such code in the database"};
      true ->
         I = ifile_fetch(CStr, Sern),
	 case I#ifile.result of
	    "success" ->
	       FId = I#ifile.id,
               lager:info("About to delete the file with ID ~p", [FId]),
               case boss_db:delete(FId) of
                  ok -> 
                     #ifile{result = "success"}; 
                  {error, DelErrMesg} ->
                     #ifile{result = DelErrMesg}
	       end;
	    _ ->
	       #ifile{result = "No such file in the database"}
	 end
   end.


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
  
