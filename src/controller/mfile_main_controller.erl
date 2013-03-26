-module(mfile_main_controller, [Req]).
-compile(export_all).


getMfileVerNum() -> 
		{ok, VerNum} = application:get_key(mfile, vsn),
		VerNum.


startEGTM() -> 
		 startEGTM ( [ X || {X,Y,Z} <- 
				application:loaded_applications(), 
				X == egtm ] ).
startEGTM([]) -> 
		 egtm:start(),
		 started;
startEGTM([egtm]) -> 
		 notstarted;
startEGTM(_) ->
		 error.


initializeForm() ->
   MfileVerNum = getMfileVerNum(),
   EGTMstartStatus = startEGTM(),
   GtmVerNum = egtm:zversion(),
   [{mfilevernum, MfileVerNum}, {mfilegtmver, GtmVerNum}].


% GET /
start('GET', []) ->
   {ok, initializeForm() }.

% assign an ID number
mfileAssignID() ->
   [LastIDstr] = egtm:order("^ZMFILE", [], backward),
   case LastIDstr of
      []  -> 1;
      Str -> {X2,_} = string:to_integer(Str), X2 + 1
   end.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   MFID = mfileAssignID(),
   MFDATA = string:concat(string:concat(Req:post_param("mfileKeyw"), "|"),
            Req:post_param("mfileDesc")),
   egtm:set("^ZMFILE", [MFID], MFDATA),
   {json, [ {mfileID, MFID}, {mfileData, MFDATA} ]}.

% 404 handler
lost('GET', []) ->
   {ok, initializeForm() }.

