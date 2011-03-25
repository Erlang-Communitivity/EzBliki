-module(timefuncs).

-export([timestamp/0]).

-author("Shahzad Bhatti <bhatti@plexobject.com>").
-contributor("William Barnhill <xmlarchitect@gmail.com>").
-reference("http://weblog.plexobject.com/?p=1594").


two_digit(X) when is_integer(X), X >= 10 ->
     integer_to_list(X);
two_digit(X) when is_integer(X), X < 10 ->
     "0" ++ integer_to_list(X).


%%%
% Returns Coordinated Universal Time (Greenwich Mean Time) time zone,
%%%
timestamp() ->
    {{_, _, _}, {_LocalHour, _LocalMin, _}} = LocalDateTime = calendar:local_time(),
    [{{Year, Month, Day}, {Hour, Min, Sec}}] =
        calendar:local_time_to_universal_time_dst(LocalDateTime),
    Z = gmt_difference(),
    integer_to_list(Year) ++ "-" ++ two_digit(Month) ++ "-" ++ two_digit(Day)
        ++ "T" ++ two_digit(Hour) ++ ":" ++ two_digit(Min) ++ ":" ++
        two_digit(Sec) ++ Z.

gmt_difference() ->
     "-08:00".
