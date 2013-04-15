-module(mfile, [Id, CreatedAt::datetime(), MfilecodeId, Sern::integer(), Keyw::string(), FileDesc::string()]).
-belongs_to(mfilecode).
-compile(export_all).

validation_tests() ->
   [
      {fun() -> mfilelib:icode_exists(CodeId) end,
       "Attempt to insert file with non-existent Code ID"}
   ]
