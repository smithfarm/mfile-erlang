-module(mfile, [Id, CreatedAt::datetime(), MfilecodeId, Sern::integer(), Keyw::string(), FileDesc::string()]).
-belongs_to(mfilecode).
-compile(export_all).

validation_tests() ->
   [
      {fun() ->
          is_list(MfilecodeId)
       end,
       "Attempt to insert file with undefined Code ID"}
      ,
      {fun() ->
          MfilecodeId =/= []
       end,
       "Attempt to insert file with empty Code ID"}
      ,
      {fun() ->
          length(Keyw) > 0
       end,
       "Attempt to insert file with empty Keywords field"}
      ,
      {fun() -> 
          Rec = boss_db:find_first(mfilecode, [{id, 'equals', MfilecodeId}]),
          R = boss_record:new(mfilecode, [{id, MfilecodeId}, {code_str, Rec:code_str()}]),
          mfiledb:icode_exists_cstr(Rec:code_str()) 
       end,
       "Attempt to insert file with non-existent Code"}
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


fetch() ->
   lager:info("mfile:fetch() looking for ~p, ~p", [MfilecodeId, Sern]),
   R = boss_db:find_first(mfile, [{mfilecode_id, 'equals', MfilecodeId},
                                  {sern, 'equals', Sern}]),
   lager:info("mfile:fetch() found record ~p", [R]),
   R.

