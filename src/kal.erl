-module(kal).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-define(GOAL_WEIGHT, 150).
-define(GOAL_DEFICIT_HIGH, 700).
-define(GOAL_DEFICIT_LOW, 400).

-record(eat, {
    kcal :: non_neg_integer(),
    fat :: non_neg_integer(),
    carb :: non_neg_integer(),
    protein :: non_neg_integer(),
    brand :: string(),
    name :: string()
}).
-record(day, {
    date :: calendar:date() | undefined,
    goal :: number() | undefined,
    burnt = 0 :: number(),
    weight = 0 :: number(),
    eats = [] :: list(#eat{}),
    rolling :: {number(), number(), number()} | undefined
}).

-spec populated(#day{}) -> boolean().
populated(#day{date = undefined}) -> false;
populated(#day{goal = undefined}) -> false;
populated(_) -> true.
populated_test() ->
    false = populated(#day{}),
    false = populated(#day{date = {2024, 5, 16}}),
    true = populated(#day{date = {2024, 5, 16}, goal = 1500}).

-spec deficit(#day{}) -> number().
deficit(#day{eats = Eats, burnt = Burnt}) ->
    lists:sum(lists:map(fun(#eat{kcal = Kcal}) -> Kcal end, Eats)) - Burnt.

-spec weekly_deficit(list(#day{})) -> {number(), number(), number()}.
weekly_deficit(Days) ->
    Sum = lists:sum(lists:map(fun deficit/1, Days)),
    Count = length(Days),
    Avg = Sum / Count,
    [Needed, Wanted] = lists:map(fun(N) -> min(0, -N * Count - Sum) end, [
        ?GOAL_DEFICIT_LOW, ?GOAL_DEFICIT_HIGH
    ]),
    {Avg, Needed, Wanted}.

grep(Pats, Line) ->
    grep(Pats, Line, maps:keys(Pats)).

grep(_, _, []) ->
    false;
grep(Pats, Line, [K | Keys]) ->
    case re:run(Line, maps:get(K, Pats), [{capture, all, binary}]) of
        nomatch -> grep(Pats, Line, Keys);
        {match, [_ | Groups]} -> {K, Groups}
    end.

-spec load(file:filename()) -> list(#day{}).
load(Path) ->
    Re = fun(P) ->
        {ok, Regex} = re:compile(P),
        Regex
    end,
    Pats = #{
        empty => Re("^\\S*$"),
        today => Re(<<"^d (?:now|щас)$"/utf8>>),
        day => Re("^d \\S+, (?P<date>\\d+ \\S+ \\d+)$"),
        take => Re("^t \\d+ / (?P<goal>\\d+) \\(-?\\d+\\) = \\d+%$"),
        burn => Re("^b (?P<burnt>\\d+) = \\d+% \\(-?\\d+\\)$"),
        macros => Re("^T \\d+:\\d+:\\d+ = \\d+ \\d+%$"),
        weight => Re(
            "^w (?P<weight>[\\d.]+)(?: \\(-?[\\d.]+\\) = -?\\d+ -?\\d+ weeks = -?[\\d.]+ -?[\\d.]+ months)?(?: = -?[\\d.]+ weekly, -?[\\d.]+ needed, -?[\\d.]+ wanted)?$"
        ),
        kcal => Re(
            "^k +\\d+% +(?P<kcal>\\d+) +(?P<fat>\\d+) +(?P<carb>\\d+) +(?P<protein>\\d+) (?P<brand>\\S+) \"(?P<name>.*)\"$"
        )
    },
    {ok, File} = file:open(Path, [read, raw, read_ahead, binary]),
    try
        loadfrom(Pats, File, [], file:read_line(File), 1, #day{})
    after
        file:close(File)
    end.

loadfrom(_, _, Days, eof, _, Day) ->
    case populated(Day) of
        true -> [Day | Days];
        false -> Days
    end;
loadfrom(Pats, File, Days, {ok, Line}, LineNo, Day) ->
    case grep(Pats, string:trim(Line, trailing)) of
        {empty, []} ->
            true = populated(Day),
            loadfrom(Pats, File, [Day | Days], file:read_line(File), LineNo + 1, #day{});
        {today, []} ->
            {Today, _} = calendar:local_time(),
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day#day{date = Today});
        {day, [Date]} ->
            {Y, M, D} = kaltime:parse(Date),
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day#day{date = {Y, M, D}});
        {take, [Goal]} ->
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day#day{
                goal = binary_to_integer(Goal)
            });
        {burn, [Burn]} ->
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day#day{
                burnt = binary_to_integer(Burn)
            });
        {macros, []} ->
            % ignore
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day);
        {weight, [Weight]} ->
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day#day{
                weight = binary_to_float(Weight)
            });
        {kcal, [Kcal, Fat, Carb, Protein, Brand, Name]} ->
            Eat = #eat{
                kcal = binary_to_integer(Kcal),
                fat = binary_to_integer(Fat),
                carb = binary_to_integer(Carb),
                protein = binary_to_integer(Protein),
                brand = Brand,
                name = Name
            },
            loadfrom(Pats, File, Days, file:read_line(File), LineNo + 1, Day#day{
                eats = Day#day.eats ++ [Eat]
            })
    end.

% expects days recent-first, returns them recent-last
calc_rolling([Day | Days], Acc) ->
    Week = [Day | lists:sublist(Days, 6)],
    Day1 = Day#day{rolling = weekly_deficit(Week)},
    calc_rolling(Days, [Day1 | Acc]);
calc_rolling([], Acc) ->
    Acc.

-record(total, {
    eaten = 0 :: integer(),
    left :: integer(),
    fat = 0 :: integer(),
    carb = 0 :: integer(),
    protein = 0 :: integer()
}).
totals([], Totals) ->
    Totals;
totals([Eat | Eats], Totals) ->
    totals(Eats, #total{
        eaten = Totals#total.eaten + Eat#eat.kcal,
        left = Totals#total.left - Eat#eat.kcal,
        fat = Totals#total.fat + Eat#eat.fat,
        carb = Totals#total.carb + Eat#eat.carb,
        protein = Totals#total.protein + Eat#eat.protein
    }).

dump(Day) ->
    #total{eaten = Eaten, left = Left, fat = Fat, carb = Carb, protein = Protein} = totals(
        Day#day.eats, #total{left = Day#day.goal}
    ),
    Daypct = Eaten / Day#day.goal * 100,
    Burnpct =
        case Eaten of
            0 -> 0;
            E -> Day#day.burnt / E * 100
        end,
    Proteinpct =
        case Eaten of
            0 -> 0;
            _ -> Protein / (Fat + Carb + Protein) * 100
        end,
    {Weekly, Needed, Wanted} = Day#day.rolling,
    io:fwrite("d ~ts~n", [kaltime:format(Day#day.date)]),
    io:fwrite("t ~b / ~b (~b) = ~b%~n", [round(X) || X <- [Eaten, Day#day.goal, Left, Daypct]]),
    io:fwrite("b ~b = ~b% (~b)~n", [round(X) || X <- [Day#day.burnt, Burnpct, deficit(Day)]]),
    io:fwrite("T ~b:~b:~b = ~b ~b%~n", [
        round(X)
     || X <- [Fat, Carb, Protein, Fat + Carb + Protein, Proteinpct]
    ]),
    case Day#day.weight of
        0 ->
            ok;
        Weight ->
            ToGo = Weight - ?GOAL_WEIGHT,
            Low = ToGo / (7 * ?GOAL_DEFICIT_LOW / 3500),
            High = ToGo / (7 * ?GOAL_DEFICIT_HIGH / 3500),
            MLow = ToGo / (4 * 7 * ?GOAL_DEFICIT_LOW / 3500),
            MHigh = ToGo / (4 * 7 * ?GOAL_DEFICIT_HIGH / 3500),
            io:fwrite(
                "w ~.1f (~b) = ~b ~b weeks = ~.1f ~.1f months = ~b weekly, ~b needed, ~b wanted~n",
                [
                    Weight,
                    round(ToGo),
                    round(Low),
                    round(High),
                    MLow,
                    MHigh,
                    round(Weekly),
                    round(Needed),
                    round(Wanted)
                ]
            )
    end,
    [dump_eat(Eat, Day#day.goal) || Eat <- Day#day.eats],
    io:fwrite("\n").

dump_eat(Eat, Goal) ->
    Pct = Eat#eat.kcal / Goal * 100,
    io:fwrite(
        "k ~3.. b% ~4.. b ~3.. b ~3.. b ~3.. b ~ts \"~ts\"~n",
        [
            round(Pct),
            round(Eat#eat.kcal),
            round(Eat#eat.fat),
            round(Eat#eat.carb),
            round(Eat#eat.protein),
            Eat#eat.brand,
            Eat#eat.name
        ]
    ).

main([Diet]) ->
    [dump(Day) || Day <- calc_rolling(load(Diet), [])],
    ok;
main([]) ->
    Diet = filename:join(os:getenv("HOME"), filename:join("txt", "diet")),
    main([Diet]);
main(_) ->
    io:format(standard_error, "kal.erl v0.1.0~n", []).
