-module(mfile, [Id, CreatedAt::datetime(), CodeId::integer(), Sern::integer(), Keyw::string(), FileDesc::string()]).
-compile(export_all).

validation_tests() ->
   [
      {fun() -> mfilelib:icode_exists(CodeId) end,
       "Attempt to insert file with non-existent Code ID"}
   ]
