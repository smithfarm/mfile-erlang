-module(mfile_main_controller, [Req]).
-compile(export_all).
-include("mfile.hrl").
-compile([{parse_transform, lager_transform}]).

% GET / 
start('GET', []) ->
   { ok, mfilelib:initializeForm() }.


% error handler (same for 'GET' and 'POST')
lost(_, _) ->
   Alist = mfilelib:initializeForm(),
   {ok, [{lost, "lost"}|Alist] }.

% GET /mainarea
mainarea('POST', []) ->
   {ok, [{filecode, Req:post_param("filecode")},
         {filesern, Req:post_param("filesern")},
         {filekeyw, Req:post_param("filekeyw")},
         {filedesc, Req:post_param("filedesc")}] }.

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

% search for records by Code+Key Words
% N.B.: Search is different from all the other commands because it requires
% preservation of state between calls - provided the search finds two or
% more records of course. If one or zero records are found, then it's just
% one function call with a return value. 
%
% For a start, we'll just find the _number_ of records that satisfy the search
% criteria and return that to the caller (i.e. JavaScript)
search('POST', []) ->
   CStr = Req:post_param("mfileCode"),
   Keyw = Req:post_param("mfileKeyw"),
   Desc = Req:post_param("mfileDesc"),
   I = mfiledb:ifile_search(CStr, Keyw, Desc),
   mfilelib:ifile_JSON(I).
   
% insert code (AJAX)
insertcode('POST', [])->
   lager:info("Calling icode_insert() with parameter ~p", [Req:post_param("cstr")]),
   I = mfiledb:icode_insert(Req:post_param("cstr")),
   mfilelib:icode_JSON(I);
insertcode(A, B) ->
   lost(A, B).

% search code (AJAX)
searchcode('POST', []) ->
   lager:info("Calling icode_search() with parameter ~p", [Req:post_param("cstr")]),
   I = mfiledb:icode_search(Req:post_param("cstr")),
   lager:info("I is ~p", [I]),
   {json, I}.
   
% fetch code (AJAX)
fetchcode('POST', []) ->
   lager:info("Calling icode_fetch() with parameter ~p", [Req:post_param("cstr")]),
   I = mfiledb:icode_fetch(Req:post_param("cstr")),
   mfilelib:icode_JSON(I).

% delete code (AJAX)
deletecode('POST', []) ->
   lager:info("Calling icode_delete() with parameter ~p", [Req:post_param("cstr")]),
   I = mfiledb:icode_delete(Req:post_param("cstr")),
   mfilelib:icode_JSON(I).
   
% test ldap (AJAX)
testldap('GET', []) ->
   lager:info("Testing LDAP functionality 2"),
   case eldap:open(["login-internal.suse.de"], [{port, 636}, {timeout, 1000}, {ssl, true}]) of
      {ok, H1} -> lager:info("Successfully opened SSL connection to LDAP server"),
                  eldap:close(H1);
      _ -> lager:info("Failed to open SSL connection to LDAP server")
   end.
   %Base = DN,
   %Scope = {scope, eldap:baseObject()},
   %Filter = {filter, eldap:present("cn")},
   %Attribute = {attributes, ["cn"]},
   %Search = [Base, Scope, Filter, Attribute].
   %eldap:search(S, Search).
