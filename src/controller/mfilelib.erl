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
   case catch list_to_integer(Sl) of
      Sn when ((is_number(Sn)) and (Sn > 0)) -> Sn;
      _ -> 0   
   end.

validate_codestr(Cf) ->
   lager:info("Entering mfilelib:validate_codestr/1"),
   icode_fetch(#icode{cstr=Cf}).

validate_codestr_and_sern(I) when is_record(I, ifile) ->
   case validate_serial_number(I#ifile.sern) of
      0 -> Result = #ifile{result = "Invalid serial number",
                           sern   = 0};
      Sn -> T = validate_codestr(I#ifile.cstr),
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


%% to be fit for insertion into the database, an mfilecode must satisfy several conditions:
%% 1. must be a list
%% 2. must have more than zero members
%% 3. must have less than nine members
%% 4. must consist of upper and lower case ASCII characters only
%% 5. must not already exist in codes table of database
%%
icode_exists(C) when is_integer(C) -> 
   I = icode_fetch(#icode{id = lists:append("mfilecode-", integer_to_list(C))}),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> true;
      _ -> false
   end;
icode_exists(C) when is_list(C) -> 
   I = icode_fetch(#icode{cstr = C}),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> true;
      _ -> false
   end.

icode_has_files(I) when is_record(I, icode) ->
   lager:info("Looking up files that have code ID: ~p", [I#icode.id]),
   Count = boss_db:count(mfile, [{code_id, 'equals', I#icode.id}]),
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
      {ok,{mfilecode, Id, T, Code, CodeDesc}} ->
         #icode{result = "success", 
	        id = Id, 
		dstr = mfilelib:timestamp_to_binary_date_only(T),
		cstr = Code};
      {error, [FirstErrMesg|_]} -> 
         #icode{result = FirstErrMesg}
   end.

%% icode_delete
icode_delete(I) when is_record(I, icode) ->
   ICode = icode_fetch(I),
   case ICode#icode.id of
      0 -> 
         #icode{result = ICode#icode.result};
      _ ->
         case icode_has_files(ICode) of
	    true -> 
	       #icode{result = lists:append(["Code ", ICode#icode.cstr, " has files; can't delete it"])};
	    false ->
               CId = lists:append("mfilecode-", integer_to_list(ICode#icode.id)),
      	       lager:info("About to delete the code with ID ~p", [CId]),
               case boss_db:delete(CId) of
	          ok -> 
	             #icode{result = "success", cstr = ICode#icode.cstr};
	          {error, DelErrMesg} ->
	             #icode{result = DelErrMesg}
	       end
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


%% mfilecodeID_strip
%% 
%% the Boss DB model returns the Code ID in the format "mfilecode-" ++ SOME_INTEGER, 
%% (e.g. "mfilecode-1"), but to be useful to us we need to strip off the "mfilecode-"
%% part and leave just the integer
%%
mfilecodeId_strip([$m,$f,$i,$l,$e,$c,$o,$d,$e,$-|T]) ->  
   list_to_integer(T).  % isn't Erlang amazing?

%% icode_fetch
%%
%% Fetch code by CId or CStr
icode_fetch(I) when is_record(I, icode) ->
   if
      I#icode.id =:= 0 ->  % fetching by CStr
         lager:info("Fetching code by CStr '~p'", [I#icode.cstr]),
         R = boss_db:find_first(mfilecode, [{code_str, 'equals', lists:map(fun uppercase_it/1, I#icode.cstr)}]);
      true ->              % fetching by CId
         lager:info("Fetching code by CId '~p'", [I#icode.id]),
         R = boss_db:find_first(mfilecode, [{id, 'equals', lists:append("mfilecode-", integer_to_list(I#icode.id))}])
   end,
   lager:info("boss_db:find_first(mfilecode) returned: ~p", [R]),
   case R of
      {mfilecode,CId,CTime,CStr,CDesc} -> 
         #icode{ result = "success", 
                 id     = mfilecodeId_strip(CId),
                 dstr   = mfilelib:timestamp_to_binary_date_only(CTime),
		 cstr   = CStr,
		 desc   = CDesc };
      undefined -> 
         #icode{ result = lists:append(["Code ", I#icode.cstr, " not found"]) };
      _  ->              
         #icode{ result = "Internal error on fetch (see log for details)" }
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
  
