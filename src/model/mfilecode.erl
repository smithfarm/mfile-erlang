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
      {fun() -> mfilelib:is_valid_cstr(CodeStr) end,
       "Malformed code"},
      {fun() -> not mfiledb:icode_exists_cstr(CodeStr) end,
       "That code is already in the database"}
   ].

%% exists_code_str/0 %% takes CodeStr
%%          %% returns true/false
exists_code_str() ->
   case boss_db:find_first(mfilecode, [{code_str, 'equals', CodeStr}]) of
      undefined -> false;
      _ -> true
   end.

%% exists_id/0 %% takes Code ID
%%          %% returns true/false
exists_id() ->
   lager:info("exists_id() Id == ~p", [Id]),
   case boss_db:find_first(mfilecode, [{id, 'equals', Id}]) of
      undefined -> false;
      _ -> true
   end.

%% fetch_by_code_str/0
fetch_by_code_str() ->
   boss_db:find_first(mfilecode, [{code_str, 'equals', CodeStr}]).

%% fetch_by_id/0
fetch_by_id() ->
   boss_db:find_first(mfilecode, [{id, 'equals', Id}]).
