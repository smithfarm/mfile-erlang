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
   [{mfilevernum, MfileVerNum}, {mfilegtmver, "No GT.M nowadays"}].


stripId(ModelId) ->
   re:replace(ModelId, "mfile-", "", [{return,list}]).

% GET /
start('GET', []) ->
   {ok, initializeForm() }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   Code = Req:post_param("mfileCode"),
   Sern = Req:post_param("mfileSern"),
   Keyw = Req:post_param("mfileKeyw"),
   Desc = Req:post_param("mfileDesc"),
   MfileRec = mfile:new(id, Code, Sern, Keyw, Desc),
   {ok,{mfile,ModelId,Code,Sern,Keyw,Desc}} = MfileRec:save(),
   {json, [ {mfileId, stripId(ModelId)}, 
            {mfileCode, Code},
	    {mfileSern, Sern},
   	    {mfileKeyw, Keyw}, 
	    {mfileDesc, Desc} ]}.

% search record (called asynchronously using AJAX)
search('POST', []) ->
   BareId = Req:post_param("mfileId"),
   SearchId = lists:append(["mfile-", BareId]),
   case boss_db:find(mfile, [{id, 'equals', SearchId}]) of
   	[{mfile,V1,V2,V3,V4,V5}] -> {json, [ {mfileId, stripId(V1)}, 
					     {mfileCode, V2},
					     {mfileSern, V3},
					     {mfileKeyw, V4}, 
					     {mfileDesc, V5} ]};
	[] -> {json, []}
   end.

% 404 handler
lost('GET', []) ->
   {ok, initializeForm() }.

