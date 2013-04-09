-module(mfile_main_controller, [Req]).
-compile(export_all).


initializeForm() ->
   % get mfile version number
   X = mfilelib:getMfileVerNum(),  
   % get PostgreSQL version number
   { ok, _, [ {DBS} ] } = boss_db:execute("SELECT version();"),
   % send to template (view/start.html)
   [{mfilevernum, X}, {mfiledbstatus, binary_to_list(DBS)}].

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
   {ok,{mfile,Id,TmpTime,CId,Sern,Keyw,FileDesc}} = MfileRec:save(),
   % The DB model sends back a timestamp in the format {{Y, M, D}, {H, M, S}} 
   % Now convert this into a binary of the format <<"YYYY-MMM-DD">> for display on-screen
   DateStr = mfilelib:timestamp_to_binary_date_only(TmpTime),
   {json, { queryResult, "success" },
          [ {mfileId,   Id}, 
   	    {mfileDate, DateStr},
            {mfileCodeId, CId},
            {mfileCode, ""},
	    {mfileSern, Sern},
   	    {mfileKeyw, Keyw}, 
	    {mfileDesc, FileDesc} ] }.

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
   {json, { queryResult, "success" },
          [ {mfilecodeId,   Id},
            {mfilecodeDate, CodeDateStr},
	    {mfilecodeCode, Code},
	    {mfilecodeDesc, CodeDesc} ] }.

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   C = Req:post_param("mfilecodeCode"), 
   case mfilelib:validate_mfilecode(C) of
   	true           -> gen_insertcode_JSON(C);
	false          -> { json, { queryResult, "Upper and lower case ASCII letters only." }, [] };
	{error, IcErr} -> { json, { queryResult, IcErr }, [] }
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

% fetch code (called asynchronously using AJAX)
fetchcode('POST', []) ->
   CodeToFetch = Req:post_param("mfilecodeCode"),
   Result = mfilelib:fetch_code_id(CodeToFetch),
   case Result of
        {ok, {V1, V2, V3, V4} } -> {json, { queryResult, "success" },
	                                  [ {mfilecodeId,  V1},
	                                    {mfilecodeDate, V2},
				            {mfilecodeCode, V3},
				            {mfilecodeDesc, V4} ] };
	undefined -> {json, { queryResult, lists:append(["Code '", CodeToFetch, "' not found"]) }, [] }
   end.
   
% error handler
lost('GET', []) ->
   {ok, initializeForm() };
lost('POST', []) ->
   {ok, initializeForm() }.
