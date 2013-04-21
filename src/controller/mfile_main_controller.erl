-module(mfile_main_controller, [Req]).
-compile(export_all).
-include("mfile.hrl").
-compile([{parse_transform, lager_transform}]).

% GET / 
start('GET', []) ->
   { ok, mfilelib:initializeForm() }.

% GET /mainarea
mainarea('POST', []) ->
   {ok, [{filecode, Req:post_param("filecode")},
         {filesern, Req:post_param("filesern")},
         {filekeyw, Req:post_param("filekeyw")},
         {filedesc, Req:post_param("filedesc")}] }.

% error handler (same for 'GET' and 'POST')
lost(_, []) ->
   Alist = mfilelib:initializeForm(),
   {ok, [{lost, "lost"}|Alist] }.

% get user confirmation for update
confirmupdate('POST', []) ->
   CStr = Req:post_param("newcode"),  % get Code string from form
   Sern = list_to_integer(Req:post_param("newsern")),    % get Serial Number from form
   I = mfiledb:ifile_fetch(CStr, Sern),
   {ok, [{newcode, Req:post_param("newcode")},
         {newsern, Req:post_param("newsern")},
         {oldkeyw, I#ifile.keyw},
         {olddesc, I#ifile.desc},
         {newkeyw, Req:post_param("newkeyw")},
         {newdesc, Req:post_param("newdesc")}] }.

% get user confirmation for delete
confirmdelete('POST', []) ->
   CStr = Req:post_param("newcode"),  % get Code string from form
   Sern = list_to_integer(Req:post_param("newsern")),    % get Serial Number from form
   I = mfiledb:ifile_fetch(CStr, Sern),
   {ok, [{newcode, Req:post_param("newcode")},
         {newsern, Req:post_param("newsern")},
         {newkeyw, I#ifile.keyw},
         {newdesc, I#ifile.desc}] }.

% insert record (called asynchronously using AJAX)
insert('POST', []) ->
   case mfiledb:icode_exists_cstr(Req:post_param("mfileCode")) of 
      false -> IFile = #ifile{result = "Code not found"};
      true  -> IFile = mfiledb:ifile_insert(Req:post_param("mfileCode"),
                                            Req:post_param("mfileKeyw"),
                                            Req:post_param("mfileDesc"))
   end,
   mfilelib:ifile_JSON(IFile).

% update record (AJAX)
update('POST', []) ->
   IFile = case mfiledb:ifile_exists(Req:post_param("mfileCode"),
                                     list_to_integer(Req:post_param("mfileSern"))) of
              false ->
                 #ifile{result = "Attempt to update non-existent file"};
              true -> 
                 mfiledb:ifile_update(Req:post_param("mfileCode"),
                                      list_to_integer(Req:post_param("mfileSern")),
                                      Req:post_param("mfileKeyw"),
                                      Req:post_param("mfileDesc"))
           end,
   mfilelib:ifile_JSON(IFile).
  
% fetch record by Code and Serial Number (called asynchronously using AJAX)
fetch('POST', []) ->
   CStr = Req:post_param("mfileCode"),  % get Code string from form
   Sern = list_to_integer(Req:post_param("mfileSern")),    % get Serial Number from form
   I = mfiledb:ifile_fetch(CStr, Sern),
   mfilelib:ifile_JSON(I).

% delete record by Code and Serial Number (called asynchronously using AJAX)
delete('POST', []) ->
   CStr = Req:post_param("mfileCode"),  % get Code string from form
   Sern = list_to_integer(Req:post_param("mfileSern")),    % get Serial Number from form
   I = mfiledb:ifile_delete(CStr, Sern),
   mfilelib:ifile_JSON(I).

% insert code (called asynchronously using AJAX)
insertcode('POST', [])->
   I = mfiledb:icode_insert(Req:post_param("mfilecodeCode")),
   mfilelib:icode_JSON(I).

% fetch code (called asynchronously using AJAX)
fetchcode('POST', []) ->
   I = mfiledb:icode_fetch(Req:post_param("mfilecodeCode")),
   mfilelib:icode_JSON(I).
   
% delete code (called asynchronously using AJAX)
deletecode('POST', []) ->
   I = mfiledb:icode_delete(Req:post_param("mfilecodeCode")),
   mfilelib:icode_JSON(I).
   
