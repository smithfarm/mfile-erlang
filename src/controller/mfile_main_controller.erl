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

% GET /
start('GET', []) ->
   {ok, initializeForm() }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   Keyw = Req:post_param("mfileKeyw"),
   Desc = Req:post_param("mfileDesc"),
   MfileRec = mfile:new(id, calendar:now_to_datetime(erlang:now()), 0, 0, Keyw, Desc),
   {ok,{mfile,Id,CrTime,CPtr,Sern,Keyw,Desc}} = MfileRec:save(),
   {{IYear,IMon,IDay},{_,_,_}} = CrTime,
   DStr = list_to_binary( [ integer_to_list(IYear), "-", 
                            integer_to_month(IMon), "-", 
			    integer_to_list(IDay) ] ),
   {json, [ {mfileId,   Id}, 
   	    {mfileDate, DStr},
            {mfileCPtr, CPtr},
	    {mfileSern, Sern},
   	    {mfileKeyw, Keyw}, 
	    {mfileDesc, Desc} ]}.

% search record (called asynchronously using AJAX)
search('POST', []) ->
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

% 404 handler
lost('GET', []) ->
   {ok, initializeForm() }.

