-module(mfile_main_controller, [Req]).
-compile(export_all).


% GET /
start('GET', []) ->
   % lager:start(),
   lager:info("Firing up mfile web UI"),
   {ok, mfilelib:initializeForm() }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   %lager:info("Parameters passed to module: ~p", [Req]),
   CStr = Req:post_param("mfileCode"),
   {{result, R}, {codeid, I}, {codestr, C}} = mfilelib:validate_codestr(CStr),
   if 
      R =:= "success" -> mfilelib:mfile_insert_JSON(I, C, Req:post_param("mfileKeyw"), Req:post_param("mfileDesc"));
      true ->            mfilelib:mfile_error_JSON(R)
   end.

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   lager:info("Parameters passed to module: ~p", [Req]),
   C = Req:post_param("mfilecodeCode"), 
   case mfilelib:mfilecode_ok_for_insert(C) of
   	yes -> mfilelib:mfilecode_insert_JSON(C);
	E   -> mfilelib:mfilecode_error_JSON(E)
   end.

% fetch record by Code and Serial Number (called asynchronously using AJAX)
fetch('POST', []) ->
    lager:info("Parameters passed to module: ~p", [Req]),
   Cf = Req:post_param("mfileCode"),  % get Code string from form
   Sf = Req:post_param("mfileSern"),  % get Serial Number from form
   Result = mfilelib:validate_codestr_and_sern(Cf, Sf),
    lager:info("validate_codestr_and_sern returned ~p", [Result]),
   { {result, R}, {codeid, I}, {codestr, C}, {sern, S} } = Result,
   % if the Code+Sern combination exists, then both 'I' (CodeId) and 'S' (Sern) will be non-zero
   Json = if
       (I =/= 0) and (S =/= 0) -> 
           % there shouldn't be more than one, but we use find_first anyway
           case boss_db:find_first(mfile, [{code_id, 'equals', I}, {sern, 'equals', S}]) of
           	{mfile,V1,V2,V3,V4,V5,V6} -> {json, [ {queryResult, "success"},
		                                      {mfileId,   V1}, 
                                                      {mfileDate, mfilelib:timestamp_to_binary_date_only(V2)},
                                                      {mfileCodeId, V3},
					              {mfileCode, C},
                                                      {mfileSern, V4},
                                                      {mfileKeyw, V5}, 
                                                      {mfileDesc, V6} ] };
        	undefined -> mfilelib:mfile_error_JSON(lists:append(["File ", C, "-", integer_to_list(S), " not found"]));
        	_         -> mfilelib:mfile_error_JSON("Internal error")
           end;
       (I =:= 0) or (S =:= 0) -> mfilelib:mfile_error_JSON(R)
   end,
   lager:info("sending back JSON: ~p", [Json]),
   Json.

% fetch code (called asynchronously using AJAX)
fetchcode('POST', []) ->
   lager:info("Parameters passed to module: ~p", [Req]),
   X = Req:post_param("mfilecodeCode"),
   Result = mfilelib:fetch_code(X),
   lager:info("fetch_code returned ~p", [Result]),
   {QR, V1, V2, V3, V4} = Result,
   {json, [ {queryResult, QR},
            {mfilecodeId, V1},
            {mfilecodeDate, V2},
            {mfilecodeCode, V3},
            {mfilecodeDesc, V4} ] }.
   
% error handler (same for 'GET' and 'POST')
lost(_, []) ->
   {ok, mfilelib:initializeForm() }.
