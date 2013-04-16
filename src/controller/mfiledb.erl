-module(mfiledb).
-compile(export_all).
-include("mfile.hrl").

%% =======================================================================
%% mfile - DB-related functions
%% =======================================================================

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
  

%% mfilecode_instantiate/1 %% takes a list
%%                         %% returns an MfilecodeRec
%%
mfilecode_instantiate([]) ->
   undefined;
mfilecode_instantiate(CStr) when is_list(CStr) ->
   mfilecode:new( id,
                  calendar:now_to_datetime(erlang:now()),
                  mfilelib:uppercase_string(CStr),
                  [] ).  % code_desc field is currently unused


mfilecode_instantiate_cid([]) ->
   undefined;
mfilecode_instantiate_cid(CId) when is_list(CId) ->
   boss_record:new(mfilecode, [{ id, CId }]).


%% mfile_instantiate/2 %% takes a list (CStr) and an integer (Sern)
%%                     %% returns an MfileRec
%%                     %% or undefined if CStr doesn't exist
mfile_instantiate([], []) ->
   undefined;
mfile_instantiate(CStr, []) ->
   CId = get_code_id(CStr),
   case CId of
      undefined -> undefined;
      _ -> boss_record:new(mfile, 
              [ { id, id },
                { created_at, calendar:now_to_datetime(erlang:now()) },
                { mfilecode_id, CId } ] )
   end;
mfile_instantiate([], Sern) ->
   undefined;
mfile_instantiate(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   CId = get_code_id(CStr),
   case CId of
      undefined -> undefined;
      _ -> boss_record:new(mfile, 
              [ { id, id },
                { created_at, calendar:now_to_datetime(erlang:now()) },
                { mfilecode_id, CId },
                { sern = Sern }] )
   end.


%% find_last_code_id/0 %% 
%%                     %% returns either a string containing the last
%%                     %% code_id/CodeID, or undefined if table empty
%%***
find_last_code_id() ->
   R = boss_db:find_last(mfilecode, []),
   case R of
      undefined ->             0;
      _ -> R:id()
   end.


%% icode_exists_cstr %% takes a CStr (list)
%%              %% returns true or false
%% This is just a wrapper for mfilecode:exists_code_str/0
icode_exists_cstr([]) ->
   false;
icode_exists_cstr(CStr) when is_list(CStr) -> 
   R = mfilecode_instantiate(CStr),
   R:exists_code_str().


%% icode_exists_cid %% takes a CId (list)
%%              %% returns true or false
%% This is just a wrapper for mfilecode:exists_id/0
icode_exists_cid([]) ->
   false;
icode_exists_cid(CId) when is_list(CId) -> 
   R = mfilecode_instantiate(CId),
   R:exists_code_str().


%% get_code_id/1 ** takes string (CStr)
%%                    ** returns a string something like "mfilecode-8"
%%                    ** if found, but don't rely on it having that format
%%                    ** if not found, returns []
%%
get_code_id([]) ->
   [];
get_code_id(CStr) when is_list(CStr) ->
   R1 = mfilecode_instantiate(CStr),
   R2 = R1:fetch(),
   case R2 of
      undefined -> [];
      _ ->         R2:id()
   end.


%% get_file_id/2 ** takes string (CStr) and integer (Sern)
%%                    ** returns a string something like "mfile-8"
%%                    ** if found, but don't rely on it having that format
%%                    ** if not found, returns []
%%
get_file_id([], []) ->
   [];
get_file_id(CStr, []) ->
   [];
get_file_id([], Sern) ->
   [];
get_file_id(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   R1 = mfile_instantiate(CStr, Sern),
   R2 = R1:fetch(),
   case R2 of
      undefined -> [];
      _ ->         R2:id()
   end.


fetch_mfilecode_rec(CStr) ->
   boss_db:find_first(mfilecode, [{code_str, 'equals', 
      mfilelib:uppercase_string(CStr)}]).


%% icode_insert/1 %% takes a code string
%%                %% returns a populated icode record
%%***
icode_insert(CStr) when is_list (CStr) ->
   true = mfilelib:is_valid_cstr(CStr),
   MfilecodeRec = mfilecode_instantiate(CStr),
   R = MfilecodeRec:save(),  
   %lager:info("MfilecodeRec:save() returned: ~p", [R]),
   icode_insert(R);
icode_insert( {ok, BossRec} ) ->
   CId = get_code_id(BossRec:code_str()),
   %lager:info("MfilecodeRec:id() == ~p", [CId]),
   #icode{result = "success", 
          id = CId,
   	  dstr = mfilelib:timestamp_to_date_string(BossRec:created_at()),
	  cstr = BossRec:code_str()};
icode_insert( {error, [FirstErrMesg|_]} ) -> 
   #icode{result = FirstErrMesg}.


%% icode_has_files/1 %% takes an mfilecode ID string
%%                   %% returns true or false
%%***
icode_has_files(CId) when is_list(CId), length(CId) > 0 ->
   lager:info("Looking up files that have code ID: ~p", [CId]),
   Count = boss_db:count(mfile, [{mfilecode_id, 'equals', CId}]),
   lager:info("Number of files found: ~p", [Count]),
   case Count of
      0 -> false;
      _ -> true
   end;
icode_has_files([]) ->
   undefined.


%% icode_delete/1 %% takes a string (file code)
%%                %% returns a populated icode record
%%***
icode_delete(CStr) when is_list(CStr) ->
   true = mfilelib:is_valid_cstr(CStr),
   BoolVal = icode_exists_cstr(CStr),
   lager:info("icode_exists_cstr(~p) returned ~p", [CStr, BoolVal]),
   case BoolVal of
      true ->
         CId = get_code_id(CStr),
         lager:info("icode_delete/1: Found mfilecode: ~p", [CId]),
         case icode_has_files(CId) of
	    true ->
	       #icode{result = "Files exist for this code: can't delete"};
	    false ->
               CodeId = get_code_id(CStr),
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


%% find_last_sern/1 %% takes a string (CId), (e.g. "mfilecode-8" but don't count
%%                  %% it having this format1)
%%                  %% returns an integer (Sern)
%%
%% Finds the current last serial number for a given MfilecodeId
%% Returns 0 if the MfilecodeId doesn't exist
%% Returns undefined if CId is empty
%%***
find_last_sern([]) -> 
   undefined;
find_last_sern(CId) when is_list(CId) ->
   lager:info("Entering find_last_sern(~p)", [CId]),
   case icode_exists_cid(CId) of
      true -> find_last_sern({exists, CStr});
      false -> 0
   end;
find_last_sern({exists, CStr}) ->
   CId = get_code_id(CStr),
   R = boss_db:find_last(mfile, [{mfilecode_id, 'equals', CId}]),
   ReturnVal = case R of
                  undefined ->           0;
                  _ -> R:sern()
               end,
   true = is_integer(ReturnVal),
   ReturnVal.


%% ifile_insert %% takes an ifile record populated with values (cid, cstr, keyw, file_desc)
%%              %% inserts new file and returns completed populated ifile record
%%    Note: this seems overly complicated?
ifile_insert(BR) when is_tuple(BR) ->
   lager:info("ifile_insert() called with tuple ~p", [BR]),
   BRwS = BR:set(sern, find_last_sern(BR:code_str()) + 1),
   lager:info("Complete MfileRec with Serial Number: ~p", [BRwS]),
   R = BRwS:save(),
   lager:info("BRwS:save() returned ~p", [R]),
   case BRwS:save() of
      {ok, BRsaved} -> {ok, BRsaved};
      {error, [ErrMesg|_]} -> ErrMesg
   end.

ifile_insert(CStr, Keyw, Desc) when is_list(CStr), length(CStr) > 0,
                                    is_list(Keyw), is_list(Desc) ->
   true = mfilelib:is_valid_cstr(CStr),
   T = mfile_instantiate(CStr, []),
   lager:info("ifile_insert() instantiated MfileRec is ~p", [T]),
   T1 = T:set([{keyw, Keyw}, {file_desc, Desc}]),
   lager:info("ifile_insert() after adding keyw and desc MfileRec is ~p", [T1]),
   Result = case boss_db:transaction(fun() -> ifile_insert(T1) end) of
      {atomic, {ok, BRsaved}} -> 
	  #ifile{ result = "success",
                  id =     BRsaved:id(),
                  dstr =   mfilelib:timestamp_to_date_string(BRsaved:created_at()),
		  cid =    BRsaved:mfilecode_id(),
                  cstr =   CStr,
                  sern =   BRsaved:sern(),
                  keyw =   BRsaved:keyw(), 
                  desc =   BRsaved:file_desc() };
      {atomic, ErrMesg} -> 
          #ifile{ result = ErrMesg };
      {aborted, Reason} -> 
          #ifile{ result = Reason }
   end,
   lager:info("ifile_insert() return value: ~p", [Result]),
   Result.


%% ifile_exists/2 %% takes a string (CStr) and an integer (Sern)
%%                %% returns true or false
%%
ifile_exists(CStr, Sern) when is_list(CStr) and is_integer(Sern) -> 
   true = mfilelib:is_valid_cstr(CStr),
   I = ifile_fetch(CStr, Sern),
   case I#ifile.result of
      "success" -> true;
      _         -> false
   end.


%% ifile_fetch/2 %% takes a CStr and Sern
%%               %% returns a populated ifile record
%%
ifile_fetch(CStr, Sern) when is_list(CStr) and is_integer(Sern) ->
   lager:info("called: ifile_fetch(~p, ~p)", [CStr, Sern]),
   CRec = 
      case mfilelib:is_valid_cstr(CStr) of 
         true ->  fetch_mfilecode_rec(CStr);
         false -> undefined
      end,
   lager:info("CRec == ~p", [CRec]),
   RetVal = 
      case CRec of 
         undefined -> #ifile{ result = "Code not found" };
         _ -> ifile_fetch(#ifile{cid =  CRec:id(),
                                 cstr = CRec:code_str(),
                                 sern = Sern})
   end.


%% ifile_fetch/1 %% takes an ifile record populated with cid, cstr, and sern
%%               %% returns a populated ifile record
ifile_fetch(I) when is_record(I, ifile) ->
   % there shouldn't be more than one, but we use find_first anyway
   R = boss_db:find_first(mfile, 
      [
         {mfilecode_id, 'equals', I#ifile.cid}, 
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
   true = mfilelib:is_valid_cstr(CStr),
   lager:info("Entering ifile_delete with arguments: ~p, ~p", [CStr, Sern]),
   case icode_exists_cstr(CStr) of
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

