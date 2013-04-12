-module(mfilecode, [Id, CreatedAt::datetime(), CodeStr::string(), CodeDesc::string()]).
-compile(export_all).

%% validation_tests for mfilecode
%% 
%% to be fit for insertion into the database, an mfilecode must satisfy several conditions:
%% 1. must be a list
%% 2. must have more than zero members
%% 3. must have less than nine members
%% 4. must consist of upper and lower case ASCII characters only
%% 5. must not already exist in codes table of database
%%
validation_tests() ->
   [
      {fun() -> is_list(CodeStr) end,
       "Not a list"},
      {fun() -> length(CodeStr) > 0 end,
       "Code is empty"},
      {fun() -> length(CodeStr) < 9 end,
       "Code string too long (max. 8 characters)"},
      {fun() -> lists:all(fun mfilelib:is_an_ASCII_letter/1, CodeStr) end,
       "Upper and lower case ASCII characters only"},
      {fun() -> not mfilelib:icode_exists(CodeStr) end,
       "That code is already in the database"}
   ].
