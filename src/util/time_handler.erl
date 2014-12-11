-module(time_handler).
-compile(export_all).


timestamp() ->
	datetime_to_timestamp( erlang:universaltime() ).
time() ->
	erlang:universaltime().	

% 时间转时间戳，格式：{{2013,11,13}, {18,0,0}}
datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
       calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

% 时间戳转时间
timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).