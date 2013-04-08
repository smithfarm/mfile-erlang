-module(mfile_main_controller, [Req]).
-compile(export_all).


getMfileVerNum() -> 
		case application:get_key(mfile, vsn) of
			{ok, Result} -> Result;
			undefined -> Result = "Undefined"
		end,
		Result.


initializeForm() ->
   MfileVerNum = getMfileVerNum(),
   {ok,_,[{DBS}]} = boss_db:execute("SELECT version();"),
   [{mfilevernum, MfileVerNum}, {mfiledbstatus, binary_to_list(DBS)}].

% GET /
start('GET', []) ->
   {ok, initializeForm() }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   MfileRec = mfile:new( id, 
                         calendar:now_to_datetime(erlang:now()), 
			 0, 
			 0, 
			 Req:post_param("mfileKeyw"), 
			 Req:post_param("mfileDesc") 
                       ),
   {ok,{mfile,Id,TmpTime,CPtr,Sern,Keyw,FileDesc}} = MfileRec:save(),
   % The DB model sends back a timestamp in the format {{Y, M, D}, {H, M, S}} 
   % Now convert this into a binary of the format <<"YYYY-MMM-DD">> for display on-screen
   DateStr = mfilelib:timestamp_to_binary_date_only(TmpTime),
   {json, [ {mfileId,   Id}, 
   	    {mfileDate, DateStr},
            {mfileCPtr, CPtr},
	    {mfileSern, Sern},
   	    {mfileKeyw, Keyw}, 
	    {mfileDesc, FileDesc} ]}.

% insertcode helper function - sends Code entered by the user to DB
gen_insertcode_JSON(GicJCode) ->
   MfilecodeRec = mfilecode:new( id,
                                 calendar:now_to_datetime(erlang:now()),
				 lists:map(fun mfilelib:uppercase_it/1, GicJCode),
				 "success"
                               ),
   {ok,{mfilecode,Id,CodeTmpTime,Code,CodeDesc}} = MfilecodeRec:save(),
   % The DB model sends back a timestamp in the format {{Y, M, D}, {H, M, S}} 
   % Now convert this into a binary of the format <<"YYYY-MMM-DD">> for display on-screen
   CodeDateStr = mfilelib:timestamp_to_binary_date_only(CodeTmpTime),
   {json, [ {mfilecodeId,   Id},
            {mfilecodeDate, CodeDateStr},
	    {mfilecodeCode, Code},
	    {mfilecodeDesc, CodeDesc} ] }.

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   % get the Code entered by the user
   IcCode = Req:post_param("mfilecodeCode"),
   % validate it and return JSON accordingly
   case mfilelib:validate_mfilecode(IcCode) of
   	true -> gen_insertcode_JSON(IcCode);
	false -> {json, [ {mfilecodeId, 0}, 
	                  {mfilecodeDate, ""}, 
		          {mfilecodeCode, ""}, 
	                  {mfilecodeDesc, "Upper and lower case ASCII letters only."} ] };
	{error, IcErr} -> { json, [ {mfilecodeId, 0},
                                    {mfilecodeDate, ""}, 
                                    {mfilecodeCode, ""}, 
                                    {mfilecodeDesc, IcErr} ] }
   end.

% fetch record (called asynchronously using AJAX)
fetch('POST', []) ->
   BareId = Req:post_param("mfileId"),
   SearchId = lists:append(["mfile-", BareId]),
   case boss_db:find(mfile, [{id, 'equals', SearchId}]) of
   	[{mfile,V1,V2,V3,V4,V5,V6}] -> {json, [ {mfileId,   V1}, 
                                                {mfileDate, V2},
                                                {mfileCode, V3},
                                                {mfileSern, V4},
                                                {mfileKeyw, V5}, 
                                                {mfileDesc, V6} ]};
	[] -> {json, []}
   end.

fetch_code(C) ->
   CodeToFetch = lists:map(fun mfilelib:uppercase_it/1, C),
   case boss_db:find(mfilecode, [{code_str, 'equals', CodeToFetch}]) of
        [{mfilecode,V1,_,V3,V4}] -> {json, [ {mfilecodeId,  V1},
	                                      {mfilecodeDate, ""},
					      {mfilecodeCode, V3},
					      {mfilecodeDesc, V4} ]};
	[] -> {json, [ {mfilecodeId,  ""},
                       {mfilecodeDate, ""},
		       {mfilecodeCode, ""},
		       {mfilecodeDesc, lists:append(["Code ", CodeToFetch, " not found"]) } ] }
   end.

% fetch code (called asynchronously using AJAX)
fetchcode('POST', []) ->
   fetch_code(Req:post_param("mfilecodeCode")).
   
% error handler
lost('GET', []) ->
   {ok, initializeForm() };
lost('POST', []) ->
   {ok, initializeForm() }.
