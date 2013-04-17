-module(mfile, [Id, CreatedAt::datetime(), MfilecodeId, Sern::integer(), Keyw::string(), FileDesc::string()]).
-belongs_to(mfilecode).
-compile(export_all).

validation_tests() ->
   [
      {fun() -> 
          Rec = boss_db:find_first(mfilecode, [{id, 'equals', MfilecodeId}]),
          R = boss_record:new(mfilecode, [{id, MfilecodeId}, {code_str, Rec:code_str()}]),
          R:exists_code_str() 
       end,
       "Attempt to insert file with non-existent Code ID"}
   ].


%% exists/0 %% takes MfilecodeId, Sern
%%          %% returns true/false
exists() ->
   case boss_db:find_first(mfile, [{mfilecode_id, 'equals', MfilecodeId},
                                   {sern, 'equals', Sern}]) of
      undefined -> false;
      _ -> true
   end.


code_str() ->
   R = boss_db:find_first(mfilecode, [{id, 'equals', MfilecodeId}]),
   case R of
      undefined -> undefined;
      _ -> R:code_str()
   end.
