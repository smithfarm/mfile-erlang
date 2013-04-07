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
   Keyw = Req:post_param("mfileKeyw"),
   Desc = Req:post_param("mfileDesc"),
   MfileRec = mfile:new(id, Keyw, Desc),
   {ok,{_,ModelId,_,_}} = MfileRec:save(),
   {json, [ {mfileId, stripId(ModelId)}, {mfileKeyw, Keyw}, {mfileDesc, Desc} ]}.

% search record (called asynchronously using AJAX)
search('POST', []) ->
   BareId = Req:post_param("mfileId"),
   SearchId = lists:append(["mfile-", BareId]),
   case boss_db:find(mfile, [{id, 'equals', SearchId}]) of
   	[{mfile,V1,V2,V3}] -> {json, [ {mfileId, stripId(V1)}, {mfileKeyw, V2}, {mfileDesc, V3} ]};
	[] -> {json, []}
   end.

% 404 handler
lost('GET', []) ->
   {ok, initializeForm() }.

