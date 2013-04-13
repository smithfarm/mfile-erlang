-module(mfile_main_controller, [Req]).
-compile(export_all).
-include("mfile.hrl").

% GET /
start('GET', []) ->
   { ok, mfilelib:initializeForm() }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
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
   Sern = list_to_integer(Req:post_param("mfileSern")),    % get Serial Number from form
   I = mfilelib:ifile_fetch(CStr, Sern),
   mfilelib:ifile_JSON(I).

% delete record by Code and Serial Number (called asynchronously using AJAX)
delete('POST', []) ->
   CStr = Req:post_param("mfileCode"),  % get Code string from form
   Sern = list_to_integer(Req:post_param("mfileSern")),    % get Serial Number from form
   I = mfilelib:ifile_delete(CStr, Sern),
   mfilelib:ifile_JSON(I).

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   I = mfilelib:icode_insert(Req:post_param("mfilecodeCode")),
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
