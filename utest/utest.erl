-module(utest).

-export([utest/1]).

% UNIT TESTS
%
% 20130329: ncutler

% call like this:
% 2> utest:utest("utest.txt").
%
% $ cat utest.txt
% {
%	fun psort:psort/1,
%	[
%		{[],[]},
%		{[1,2,3],[1,2,3]}
%	]
% }.
% $
%
% where "psort:psort/1" is the function to test
% and then list of 2-tuples {Input, Desired_output}


% utest/1
% -------
% Reads a file into a binary and performs operations on it

% Filename is provided by the user by calling, e.g.
% utest:utest("utest.txt").
utest(Filename) when is_list(Filename) ->
	{ok, {F, List_of_test_cases}} = file:script(Filename),
	run_tests(F, 0, List_of_test_cases).


% run_tests/3
% -----------
run_tests(F, 0, L) -> 
	io:format("Function is ~p.~n", [F]),
	io:format("Received list of ~p lists.~n", [length(L)]),
	run_tests(F, 1, L);
run_tests(_, _, []) ->
	io:format("Last test.~n");
run_tests(F, N, [{H1,H2}|T]) ->
	utest(F, N, H1, H2),
	run_tests(F, N+1, T).
	

% utest/4
% -------
utest(F, N, Input, Desired) ->
	io:format("Test #~p", [N]),
	Output = F(Input),
	uresult(ucomp(Output, Desired)).


% ucomp/2
% -------
ucomp(Output, Desired) when Output == Desired -> upass;
ucomp(_, _) -> ufail.


% uresult/1
% ---------
uresult(upass) -> io:format(" passed.~n");
uresult(ufail) -> io:format(" failed.~n").
