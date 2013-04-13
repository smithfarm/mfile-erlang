-module(mfilelib).
-compile(export_all).
-include("mfile.hrl").


%
% get mfile version number and DB backend version info
%
initializeForm() ->
   X = mfilelib:getMfileVerNum(),  
   % get PostgreSQL version number
   { ok, _, [ {DBS} ] } = boss_db:execute("SELECT version();"),
   % prep for sending to template (view/start.html)
   [{mfilevernum, X}, {mfiledbstatus, binary_to_list(DBS)}].

%
% Find the current last serial number for a given CodeId
%
find_last_sern(CId) ->
   R = boss_db:find_last(mfile, [{code_id, 'equals', CId}]),
   lager:info("boss_db:find_last(mfile) returned: ~p", [R]),
   case R of
      {mfile,_,_,_,S,_,_} -> S;
      undefined ->           0
   end.

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
   try 
      list_to_integer(Sl) 
   of
      Sn when (is_number(Sn)) and (Sn > 0) -> Sn
   catch
      _:_ -> 0   
   end.

validate_codestr_and_sern(I) when is_record(I, ifile) ->
   case validate_serial_number(I#ifile.sern) of
      0 -> Result = #ifile{result = "Invalid serial number",
                           sern   = 0};
      Sn -> T = icode_fetch(I#ifile.cstr),
            Result = #ifile{result = T#icode.result,
	                    cstr   = T#icode.cstr,
		 	    cid    = T#icode.id,
			    sern   = Sn}
   end,
   Result.

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


%%
%% icode_exists %% takes a CId (integer) or CStr (list)
%%              %% returns true or false
%%
icode_exists(C) when is_integer(C) -> 
   I = icode_fetch(C),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> true;
      _ -> false
   end;
icode_exists(C) when is_list(C) -> 
   I = icode_fetch(C),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> true;
      _ -> false
   end.

icode_get_bossid(CStr) when is_list(CStr) ->
   I = icode_fetch(CStr),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> I#icode.id;
      _ -> 0
   end.

%% takes an integer, e.g. 8, not "mfilecode-8"
icode_has_files(CId) when is_integer(CId) ->
   lager:info("Looking up files that have code ID: ~p", [CId]),
   Count = boss_db:count(mfile, [{code_id, 'equals', CId}]),
   lager:info("Number of files found: ~p", [Count]),
   case Count of
      0 -> false;
      _ -> true
   end.

%% icode_JSON
icode_JSON(I) when is_record(I, icode) ->
   {json, [ {queryResult,   I#icode.result},
            {mfilecodeId,   I#icode.id},
            {mfilecodeDate, I#icode.dstr},
	    {mfilecodeCode, I#icode.cstr},
	    {mfilecodeDesc, I#icode.desc} ] }.

%% icode_insert
icode_insert(I) when is_record(I, icode) ->
   MfilecodeRec = mfilecode:new( id,
                                 calendar:now_to_datetime(erlang:now()),
				 lists:map(fun mfilelib:uppercase_it/1, I#icode.cstr),
				 [] ),
   case MfilecodeRec:save() of
      {ok,{mfilecode, Id, T, Code, _}} ->
         #icode{result = "success", 
	        id = Id, 
		dstr = mfilelib:timestamp_to_binary_date_only(T),
		cstr = Code};
      {error, [FirstErrMesg|_]} -> 
         #icode{result = FirstErrMesg}
   end.

%% ifile_exists
ifile_exists(I) when is_record(I, ifile) ->
   {no, I}.

mfilecodeId_split("mfilecode-"++T) ->
   list_to_integer(T).

icode_delete(CStr) when is_list(CStr) ->
   lager:info("Entering icode_delete with argument: ~p", [CStr]),
   BossRec = boss_db:find_first(mfilecode, [{code_str, 'equals', CStr}]),
   case BossRec of
      undefined ->
         #icode{result = "No such code in the database"};
      _ ->
         lager:info("find_first(mfilecode) returned record: ~p", [BossRec]),
	 CId = mfilecodeId_split(BossRec:id()),
         lager:info("The record has ID: ~p", [CId]),
         case icode_has_files(CId) of
	    true ->
	       #icode{result = "Files exist for this code: can't delete"};
	    false ->
               lager:info("About to delete the file code with ID ~p", [CId]),
               case boss_db:delete(BossRec:id()) of
                  ok -> 
                     #icode{result = "success"}; 
                  {error, DelErrMesg} ->
                     #icode{result = DelErrMesg}
	       end
	 end
   end.

ifile_delete(I) when is_record(I, ifile) ->
   lager:info("Entering ifile_delete with argument: ~p", [I]),
   case icode_exists(I#ifile.cstr) of
      false ->
         #ifile{result = "No such code in the database"};
      true ->
         FId = lists:append("mfile-", integer_to_list(I#ifile.id)),
         lager:info("About to delete the file with ID ~p", [FId]),
         case boss_db:delete(FId) of
            ok -> 
               #ifile{result = "success"};  % !!!!!........XXXXWWWWWXXXXX............
            {error, DelErrMesg} ->
               #ifile{result = DelErrMesg}
	 end
   end.

%% ifile_insert
%% attempt to insert mfile record and produce JSON output
ifile_insert(I) when is_record(I, ifile) ->
   MfileRec = boss_record:new( mfile, [ {id, id},
                                        {created_at, calendar:now_to_datetime(erlang:now())}, 
					{code_id, I#ifile.cid},
					{sern, (find_last_sern(I#ifile.cid)+1)},  % not atomic, unfortunately!
			                {keyw, I#ifile.keyw}, 
			                {file_desc, I#ifile.desc} ] 
                              ),
   lager:info("boss_record:new(mfile) returned: ~p", [MfileRec]),
   case MfileRec:save() of
      {ok,{mfile, Id, T, CId, Sern, Keyw, FileDesc}} ->
	  #ifile{ result = "success",
                  id =     Id, 
                  dstr =   mfilelib:timestamp_to_binary_date_only(T),
		  cid =    CId,
                  cstr =   I#ifile.cstr,
                  sern =   Sern,
                  keyw =   Keyw, 
                  desc =   FileDesc };
      {error, _} -> 
          #ifile{result = "Internal error on insert (see log for details)"}
   end.


%% mfilecodeId_strip
%% 
%% the Boss DB model returns the Code ID in the format "mfilecode-" ++ SOME_INTEGER, 
%% (e.g. "mfilecode-1"), but since the mfiles table links to the mfilecodes table
%% using an integer CodeId field, we need to strip off the "mfilecode-" part and 
%% leave just the integer
%%
%% Note the special-case use of the operator '++' on the left-hand side of the
%% pattern-matching operator. This is a kind of short-hand for:
%% [$m, $f, $i, $l, $e, $c, $o, $d, $e, $- | T]
%%
mfilecodeId_strip("mfilecode-"++T) ->  
   list_to_integer(T).  % isn't Erlang amazing?

%%
%% icode_fetch %% Fetch code by CId or CStr
%%
%% Returns an icode record
%%
icode_fetch(CId) when is_number(CId) ->
   R = boss_db:find_first(mfilecode, [{id, 'equals', lists:append("mfilecode-", integer_to_list(CId))}]),
   icode_fetch(R);
icode_fetch(CStr) when is_list(CStr) ->
   R = boss_db:find_first(mfilecode, [{code_str, 'equals', lists:map(fun uppercase_it/1, CStr)}]),
   icode_fetch(R);
icode_fetch(BossRec) ->
   lager:info("icode_fetch(BossRec) called with argument: ~p", [BossRec]),
   case BossRec of
      undefined -> 
         #icode{ result = "Code not found" };
      _ ->
         #icode{ result = "success", 
                 id     = mfilecodeId_strip(BossRec:id()),
                 dstr   = timestamp_to_binary_date_only(BossRec:created_at()),
		 cstr   = BossRec:code_str(),
		 desc   = BossRec:code_desc() }
   end.

%% ifile_fetch
ifile_fetch(I) when is_record(I, ifile) ->
   IFile = validate_codestr_and_sern(I),
   lager:info("validate_codestr_and_sern returned ~p", [IFile]),
   case IFile#ifile.result of
      "success" -> 
           % there shouldn't be more than one, but we use find_first anyway
	   lager:info("Calling boss_db:find_first(mfile) with ~p", [IFile]),
	   R = boss_db:find_first(mfile, [{code_id, 'equals', IFile#ifile.cid}, {sern, 'equals', IFile#ifile.sern}]),
	   lager:info("boss_db:find_first(mfile) returned: ~p", [R]),
           case R of
           	{mfile,V1,V2,_,V4,V5,V6} -> 
		   #ifile{ result = "success",
		           id     = V1,
		           dstr   = mfilelib:timestamp_to_binary_date_only(V2),
                           cid    = IFile#ifile.cid,
		   	   cstr   = IFile#ifile.cstr,
                           sern   = V4,
                           keyw   = V5, 
                           desc   = V6};
        	undefined -> 
                   UndefRes = lists:append(["File ", IFile#ifile.cstr, "-", integer_to_list(IFile#ifile.sern), " not found"]),
		   lager:info("Returning error ~p", [UndefRes]),
		   #ifile{ result = UndefRes };
        	_ -> 
		   #ifile{ result = "Internal error on fetch (see log for details)" }
           end;
       _ ->
          IFile
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
  
