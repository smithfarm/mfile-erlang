-module(mfile_main_controller, [Req]).
-compile(export_all).
-include("mfile.hrl").

% GET /
start('GET', []) ->
   lager:info("Firing up mfile web UI"),
   {ok, mfilelib:initializeForm() }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   lager:info("Entering main insert function"),
   C = mfilelib:icode_fetch(Req:post_param("mfileCode")),
   case C#icode.result of 
      "success" -> I = #ifile{cid  = C#icode.id,
                              cstr = C#icode.cstr,
			      keyw = Req:post_param("mfileKeyw"), 
			      desc = Req:post_param("mfileDesc")},
                   IFile = mfilelib:ifile_insert(I);
      _         -> IFile = #ifile{result = C#icode.result}
   end,
   mfilelib:ifile_JSON(IFile).

% fetch record by Code and Serial Number (called asynchronously using AJAX)
fetch('POST', []) ->
   CStr = Req:post_param("mfileCode"),  % get Code string from form
   Sn = list_to_integer(Req:post_param("mfileSern")),    % get Serial Number from form
   I = mfilelib:ifile_fetch(#ifile{cstr = CStr, sern = Sn}),
   mfilelib:ifile_JSON(I).

% delete record by Code and Serial Number (called asynchronously using AJAX)
delete('POST', []) ->
   CStr = Req:post_param("mfileCode"),  % get Code string from form
   Sn = list_to_integer(Req:post_param("mfileSern")),    % get Serial Number from form
   I = mfilelib:ifile_delete(#ifile{cstr = CStr, sern = Sn}),
   mfilelib:ifile_JSON(I).

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   CStr = Req:post_param("mfilecodeCode"), 
   I = mfilelib:icode_insert(#icode{cstr = CStr}),
   mfilelib:icode_JSON(I).

% fetch code (called asynchronously using AJAX)
fetchcode('POST', []) ->
   I = mfilelib:icode_fetch(Req:post_param("mfilecodeCode")),
   mfilelib:icode_JSON(I).
   
% delete code (called asynchronously using AJAX)
deletecode('POST', []) ->
   I = mfilelib:icode_delete(Req:post_param("mfilecodeCode")),
   mfilelib:icode_JSON(I).
   
% error handler (same for 'GET' and 'POST')
lost(_, []) ->
   {ok, mfilelib:initializeForm() }.
