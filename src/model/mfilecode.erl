-module(mfilecode, [Id, CreatedAt::datetime(), CodeId::integer(), CodeStr::string(), CodeDesc::string()]).
-compile(export_all).

%% validation_tests for mfilecode
%% 
%% to be fit for insertion into the database, a CodeStr must satisfy several conditions:
%% 1. must be a list
%% 2. must have more than zero members
%% 3. must have less than nine members
%% 4. must consist of upper and lower case ASCII characters only
%% 5. must not already exist in codes table of database
%%
%% 6. also, a CodeId must be provided
%% 7. and that CodeId must be greater than zero
%%
validation_tests() ->
   [
      {fun() -> length(CodeStr) > 0 end,
       "Code is empty"},
      {fun() -> length(CodeStr) < 9 end,
       "Code string too long (max. 8 characters)"},
      {fun() -> lists:all(fun mfilelib:is_an_ASCII_letter/1, CodeStr) end,
       "Upper and lower case ASCII characters only"},
      {fun() -> not mfilelib:icode_exists(CodeStr) end,
       "That code is already in the database"},
      {fun() -> is_integer(CodeId) end,
       "Code ID is not an integer"},
      {fun() -> CodeId > 0 end,
       "Code ID is not greater than zero"}
   ].
