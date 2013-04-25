-module(mfilecode, [Id, CreatedAt::datetime(), CodeStr::string(), CodeDesc::string()]).
-has({mfiles, many, [{order_by, sern}]}).
-compile(export_all).


%% validation_tests for mfilecode
%% 
validation_tests() ->
   [
      {fun() -> length(CodeStr) > 0 end,
       "Code is empty"},
      {fun() -> length(CodeStr) < 9 end,
       "Code string too long (max. 8 characters)"},
      {fun() -> mfilelib:is_valid_db_cstr(CodeStr) end,
       "Malformed code"},
      {fun() -> not mfiledb:icode_exists_cstr(CodeStr) end,
       "That code is already in the database"}
   ].

%% fetch_by_id/0
fetch_by_id() ->
   boss_db:find_first(mfilecode, [{id, 'equals', Id}]).

