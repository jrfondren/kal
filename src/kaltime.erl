-module(kaltime).
-export([parse/1, format/1]).

parse(DMY) ->
    [Day, Month, Year] = binary:split(DMY, <<" ">>, [global]),
    {binary_to_integer(Year), month(Month), binary_to_integer(Day)}.

format({Year, Month, Day} = YMD) ->
    Weekday = calendar:day_of_the_week(YMD),
    io_lib:fwrite(
        "~ts, ~2..0b ~ts ~b",
        [weekday(Weekday), Day, month_string(Month), Year]
    ).

month(<<"Jan">>) -> 1;
month(<<"Feb">>) -> 2;
month(<<"Mar">>) -> 3;
month(<<"Apr">>) -> 4;
month(<<"May">>) -> 5;
month(<<"Jun">>) -> 6;
month(<<"Jul">>) -> 7;
month(<<"Aug">>) -> 8;
month(<<"Sep">>) -> 9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12;
month(<<"Янв"/utf8>>) -> 1;
month(<<"Фев"/utf8>>) -> 2;
month(<<"Мар"/utf8>>) -> 3;
month(<<"Апр"/utf8>>) -> 4;
month(<<"Май"/utf8>>) -> 5;
month(<<"Июн"/utf8>>) -> 6;
month(<<"Июл"/utf8>>) -> 7;
month(<<"Авг"/utf8>>) -> 8;
month(<<"Сен"/utf8>>) -> 9;
month(<<"Окт"/utf8>>) -> 10;
month(<<"Ноя"/utf8>>) -> 11;
month(<<"Дек"/utf8>>) -> 12.

-ifdef(RUSSIAN).
month_string(1) -> <<"Янв"/utf8>>;
month_string(2) -> <<"Фев"/utf8>>;
month_string(3) -> <<"Мар"/utf8>>;
month_string(4) -> <<"Апр"/utf8>>;
month_string(5) -> <<"Май"/utf8>>;
month_string(6) -> <<"Июн"/utf8>>;
month_string(7) -> <<"Июл"/utf8>>;
month_string(8) -> <<"Авг"/utf8>>;
month_string(9) -> <<"Сен"/utf8>>;
month_string(10) -> <<"Окт"/utf8>>;
month_string(11) -> <<"Ноя"/utf8>>;
month_string(12) -> <<"Дек"/utf8>>.

weekday(1) -> <<"Понедельник"/utf8>>;
weekday(2) -> <<"Вторник"/utf8>>;
weekday(3) -> <<"Среда"/utf8>>;
weekday(4) -> <<"Четверг"/utf8>>;
weekday(5) -> <<"Пятница"/utf8>>;
weekday(6) -> <<"Суббота"/utf8>>;
weekday(7) -> <<"Воскресенье"/utf8>>.
-else.
month_string(1) -> <<"Jan">>;
month_string(2) -> <<"Feb">>;
month_string(3) -> <<"Mar">>;
month_string(4) -> <<"Apr">>;
month_string(5) -> <<"May">>;
month_string(6) -> <<"Jun">>;
month_string(7) -> <<"Jul">>;
month_string(8) -> <<"Aug">>;
month_string(9) -> <<"Sep">>;
month_string(10) -> <<"Oct">>;
month_string(11) -> <<"Nov">>;
month_string(12) -> <<"Dec">>.

weekday(1) -> <<"Monday">>;
weekday(2) -> <<"Tuesday">>;
weekday(3) -> <<"Wednesday">>;
weekday(4) -> <<"Thursday">>;
weekday(5) -> <<"Friday">>;
weekday(6) -> <<"Saturday">>;
weekday(7) -> <<"Sunday">>.
-endif.
