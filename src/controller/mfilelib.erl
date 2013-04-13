-module(mfilelib).
-compile(export_all).
-include("mfile.hrl").


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


%% find_last_sern/1 %% takes an integer (CId)
%%                  %% returns an integer (Sern)
%%
%% Finds the current last serial number for a given CodeId
%%
find_last_sern(CId) when is_integer(CId) ->
   R = boss_db:find_last(mfile, [{code_id, 'equals', CId}]),
   lager:info("boss_db:find_last(mfile) returned: ~p", [R]),
   case R of
      {mfile,_,_,_,S,_,_} -> S;
      undefined ->           0
   end.

%% validate_serial_number/1 %% takes a value (integer, binary, string)
%%                          %% returns 0 if it's not a natural number
%%                          %% or integer if it is
%% 
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

%% validate_codestr_and_sern/2 %% takes a string (CStr) and an integer (Sern)
%%                             %% returns a populated icode instance
%%
validate_codestr_and_sern(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   case validate_serial_number(Sern) of
      0 -> Result = #ifile{result = "Invalid serial number or other weirdness",
                           sern   = 0};
      Sern -> T = icode_fetch(CStr),
              Result = #ifile{result = T#icode.result,
	                      cstr   = T#icode.cstr,
	    	 	      cid    = T#icode.id,
			      sern   = Sern}
   end,
   Result.


%% integer_to_month %% takes an integer in the range 1-12
%%                  %% returns list containing a three-letter representation 
%%                  %% of the month
%%
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


%% timestamp_to_binary_date_only %% takes a timestamp {{Y, M, D}, {H, M, S}}
%%                               %% returns binary <<"YYYY-MMM-DD">>
%%
timestamp_to_binary_date_only(Timest) ->
   {{Y, M, D},{_,_,_}} = Timest,
   list_to_binary( [ integer_to_list(Y), "-", 
                     integer_to_month(M), "-", 
                     integer_to_list(D) ] ).


%% uppercase_it %% takes an integer, deducts 32 if it corresponds to a lowercase letter
%%              %% returns an integer
%%
uppercase_it(E) ->
   case lists:member(E, lists:seq($a, $z)) of
      true -> E - 32;
      false -> E
   end.


%% is_an_ASCII_letter %% takes a integer
%%                    %% returns true or false
%%
is_an_ASCII_letter(X) ->
    Uppers = lists:seq($A, $Z),
    Lowers = lists:seq($a, $z),
    lists:member(X, Uppers) or lists:member(X, Lowers).


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

%%
%% NOT USED
%%
icode_get_bossid(CStr) when is_list(CStr) ->
   I = icode_fetch(CStr),
   lager:info("icode_fetch returned ~p", [I]),
   case I#icode.result of
      "success" -> I#icode.id;
      _ -> 0
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


%% icode_insert/1 %% takes a code string
%%                %% returns a populated icode record
%%
icode_insert(CStr) when is_list (CStr) ->
   MfilecodeRec = mfilecode:new( id,
                                 calendar:now_to_datetime(erlang:now()),
				 lists:map(fun mfilelib:uppercase_it/1, CStr),
				 [] ),   % code_desc field is currently unused
   R = MfilecodeRec:save(),   % N.B.: save() does NOT update Id field!!
   %lager:info("MfilecodeRec:save() returned: ~p", [R]),
   icode_insert(R);
% This (having to write out the entire mfilecode tuple) is sub-optimal
icode_insert( {ok, {mfilecode, CId, _, _, _}} ) ->
   lager:info("MfilecodeRec:id() == ~p", [CId]),
   #icode{result = "success", 
          id = CId,
   	  dstr = mfilelib:timestamp_to_binary_date_only(MfilecodeRec:created_at()),
	  cstr = MfilecodeRec:code_str()};
icode_insert( {error, [FirstErrMesg|_]} ) -> 
   #icode{result = FirstErrMesg}


%% ifile_exists/2 %% takes a string (CStr) and an integer (Sern)
%%                %% returns true or false
%%
ifile_exists(CStr, Sern) when is_list(CStr) and is_integer(Sern) -> 
   I = ifile_fetch(#ifile{cstr = CStr, sern = Sern}),
   case I#ifile.result of
      "success" -> true;
      _         -> false
   end.


%% icode_delete/1 %% takes a string (file code)
%%                %% returns a populated icode record
%%
icode_delete(CStr) when is_list(CStr) ->
   lager:info("Entering icode_delete with argument: ~p", [CStr]),
   BossRec = boss_db:find_first(mfilecode, [{code_str, 'equals', CStr}]),
   case BossRec of
      undefined ->
         #icode{result = "No such code in the database"};
      _ ->
         lager:info("find_first(mfilecode) returned record: ~p", [BossRec]),
	 CId = mfilecodeId_strip(BossRec:id()),
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


%% ifile_delete/2 %% takes a string (CStr) and an integer (Sern)
%%                %% returns a populated ifile record
%%
ifile_delete(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   lager:info("Entering ifile_delete with arguments: ~p, ~p", [CStr, Sern]),
   case icode_exists(CStr) of
      false ->
         #ifile{result = "No such code in the database"};
      true ->
         I = ifile_fetch(#ifile{cstr = CStr, sern = Sern}),
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


%% ifile_insert %% takes an ifile record populated with values (cid, cstr, keyw, file_desc)
%%              %% inserts new file and returns completed populated ifile record
%%              
ifile_insert(I) when is_record(I, ifile) ->
   MfileRec = boss_record:new( mfile, [ 
                                        {created_at, calendar:now_to_datetime(erlang:now())}, 
					{code_id,    I#ifile.cid},
					{sern,       find_last_sern(I#ifile.cid) + 1},  % not atomic, unfortunately!
			                {keyw,       I#ifile.keyw}, 
			                {file_desc,  I#ifile.desc} 
			              ] 
                             ),
   lager:info("boss_record:new(mfile) returned: ~p", [MfileRec]),
   case MfileRec:save() of   %% N.B.: Does not update id field!!
      {ok, {mfile, AllocatedId, _, _, _, _, _}} ->
	  #ifile{ result = "success",
                  id =     AllocatedId,
                  dstr =   mfilelib:timestamp_to_binary_date_only(MfileRec:created_at()),
		  cid =    MfileRec:code_id(),
                  cstr =   I#ifile.cstr,
                  sern =   MfileRec:sern(),
                  keyw =   MfileRec:keyw(), 
                  desc =   MfileRec:file_desc() };
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
mfilecodeId_strip("mfilecode-" ++ T) ->                     % isn't Erlang amazing?
   list_to_integer(T).  


%% icode_fetch/1 %% Takes either a CId or a CStr
%%               %% Fetches it from the DB and returns a populated icode record
%%
icode_fetch(CId) when is_number(CId) ->
   R = boss_db:find_first(mfilecode, [{id, 
                                      'equals', 
				      lists:append("mfilecode-", integer_to_list(CId))} ]),
   icode_fetch(R);
icode_fetch(CStr) when is_list(CStr) ->
   R = boss_db:find_first(mfilecode, [{code_str, 
                                       'equals', 
				       lists:map(fun uppercase_it/1, CStr)} ]),
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


%% ifile_fetch/1 %% takes an ifile record populated with cstr and sern
%%               %% returns a populated ifile record
%%
ifile_fetch(I) when is_record(I, ifile) ->
   IFile = validate_codestr_and_sern(I#ifile.cstr, I#ifile.sern),
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
  
