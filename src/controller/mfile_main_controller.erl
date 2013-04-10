-module(mfile_main_controller, [Req]).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).


initializeForm() ->
   % get mfile version number
   X = mfilelib:getMfileVerNum(),  
   % get PostgreSQL version number
   { ok, _, [ {DBS} ] } = boss_db:execute("SELECT version();"),
   % send to template (view/start.html)
   [{mfilevernum, X}, {mfiledbstatus, binary_to_list(DBS)}].

% GET /
start('GET', []) ->
   lager:start(),
   lager:info("Firing up mfile web UI"),
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
   {json, [ {queryResult, "success"},
            {mfileId,     Id}, 
   	    {mfileDate,   DateStr},
            {mfileCodeId, CId},
            {mfileCode,   ""},
	    {mfileSern,   Sern},
   	    {mfileKeyw,   Keyw}, 
	    {mfileDesc,   FileDesc} ] }.

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
   {json, [ {queryResult,  "success"},
            {mfilecodeId,   Id},
            {mfilecodeDate, CodeDateStr},
	    {mfilecodeCode, Code},
	    {mfilecodeDesc, CodeDesc} ] }.

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   C = Req:post_param("mfilecodeCode"), 
   case mfilelib:mfilecode_ok_for_insert(C) of
   	yes         -> gen_insertcode_JSON(C);
	ErrorString -> { json, [ { queryResult,   ErrorString }, 
	                         { mfilecodeID,   0 }, 
				 { mfilecodeDate, ""}, 
				 { mfilecodeCode, ""},
				 { mfilecodeDesc, ""} ] }
   end.

% fetch record by Code and Serial Number (called asynchronously using AJAX)
fetch('POST', []) ->
   Cf = Req:post_param("mfileCode"),  % get Code string from form
   Sf = Req:post_param("mfileSern"),  % get Serial Number from form

   Result = mfilelib:validate_codestr_and_sern(Cf, Sf),
   lager:info("validate_codestr_and_sern returned ~p", [Result]),
   { {result, R}, {codeid, I}, 
     {codestr, C}, {sern, S} } = Result,
   % if the Code exists in the database, I will be something other than 0
   Json = if
       (I /= 0) and (S /= 0) -> 
           case boss_db:find(mfile, [{code_id, 'equals', I}, {sern, 'equals', S}]) of
           	[{mfile,V1,V2,V3,V4,V5,V6}] -> {json, [ {queryResult, "success"},
		                                        {mfileId,   V1}, 
                                                        {mfileDate, V2},
                                                        {mfileCode, V3},
                                                        {mfileSern, V4},
                                                        {mfileKeyw, V5}, 
                                                        {mfileDesc, V6} ] };
        	[] -> {json, [ {queryResult, lists:append(["Code '", C, "' not found"]) }, [] ] }
           end;
       not ((I /= 0) and (S /= 0)) -> {json, [ { queryResult, R }, [] ] }
   end,
   lager:info("sending back JSON: ~p", [Json]),
   Json.

% fetch code (called asynchronously using AJAX)
fetchcode('POST', []) ->
   X = Req:post_param("mfilecodeCode"),
   Result = mfilelib:fetch_code(X),
   lager:info("fetch_code returned ~p", [Result]),
   {QR, V1, V2, V3, V4} = Result,
   {json, [ {queryResult, QR},
            {mfilecodeId, V1},
            {mfilecodeDate, V2},
            {mfilecodeCode, V3},
            {mfilecodeDesc, V4} ] }.
   
% error handler
lost('GET', []) ->
   {ok, initializeForm() };
lost('POST', []) ->
   {ok, initializeForm() }.
