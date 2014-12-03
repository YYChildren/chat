-module(test).
-compile(export_all).

start(SB) ->
	io:format("hello"),
	io:format(SB),
	io:format().
	