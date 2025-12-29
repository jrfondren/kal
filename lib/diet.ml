let russian = try Unix.getenv "LANG" = "ru_RU.UTF-8" with Not_found -> false

type eat = {
  kcal : int;
  fat : int;
  carb : int;
  protein : int;
  brand : string;
  name : string;
}

type day = {
  date : float;
  goal : int;
  eats : eat list;
}

let months =
  "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec "
  ^ "Янв Фев Мар Апр Май Июн Июл Авг Сен Окт Ноя Дек"
  |> String.split_on_char ' '
  |> Array.of_list

let weekdays =
  "Sunday Monday Tuesday Wednesday Thursday Friday Saturday "
  ^ "Воскресенье Понедельник Вторник Среда Четверг Пятница Суббота"
  |> String.split_on_char ' '
  |> Array.of_list

let month_of_int n = months.(n + if russian then 12 else 0)
let weekday_of_int n = weekdays.(n + if russian then 7 else 0)

let readline days =
  let open Scanf in
  function
  | "d now" -> {date = Unix.time (); goal = 0; eats = []} :: days
  | "" -> days
  | line -> (
    match (String.get line 0, days) with
    | 'd', _ ->
      sscanf line "d %_s %d %s %d" (fun tm_mday mon year ->
          let date, _ =
            Unix.(
              mktime
                {
                  tm_sec = 0;
                  tm_min = 0;
                  tm_hour = 0;
                  tm_mday;
                  tm_mon =
                    (Array.find_index (( = ) mon) months |> Option.get) mod 12;
                  tm_year = year - 1900;
                  tm_wday = 0;
                  tm_yday = 0;
                  tm_isdst = false;
                })
          in
          {date; goal = 0; eats = []} :: days)
    | 't', hd :: days ->
      sscanf line "t %_d / %d (%_d) = %_d%%" (fun goal ->
          {hd with goal} :: days)
    | 'w', _ | 'b', _ | 'T', _ -> days
    | 'k', hd :: days ->
      sscanf line {|k %_d%% %d %d %d %d %s "%s@"|}
        (fun kcal fat carb protein brand name ->
          {hd with eats = {kcal; fat; carb; protein; brand; name} :: hd.eats}
          :: days)
    | _, _ -> failwith ("bad line: " ^ line))

let load path =
  In_channel.with_open_text path (fun inp ->
      let rec loop days =
        match In_channel.input_line inp with
        | Some line -> loop (readline days line)
        | None -> days
      in
      loop [])

let dump days =
  let open Printf in
  List.rev days
  |> List.iter (fun d ->
      let tm = Unix.gmtime d.date in
      let kcal, fat, carb, protein =
        List.fold_left
          (fun (k, f, c, p) e ->
            (k + e.kcal, f + e.fat, c + e.carb, p + e.protein))
          (0, 0, 0, 0) d.eats
      in
      let macros = fat + carb + protein in
      let fatp, carbp, proteinp =
        try (100 * fat / macros, 100 * carb / macros, 100 * protein / macros)
        with Division_by_zero -> (0, 0, 0)
      in
      let eatpct = 100 * kcal / d.goal in
      printf "d %s, %d %s %d\n"
        (weekday_of_int tm.tm_wday)
        tm.tm_mday (month_of_int tm.tm_mon) (1900 + tm.tm_year);
      printf "t %d / %d (%d) = %d%%\n" kcal d.goal (kcal - d.goal) eatpct;
      printf "T %d:%d:%d = %d:%d:%d%%\n" fat carb protein fatp carbp proteinp;
      List.rev d.eats
      |> List.iter (fun e ->
          let pct = try 100 * e.kcal / kcal with Division_by_zero -> 0 in
          printf "k %3d%% %4d %3d %3d %3d %s \"%s\"\n" pct e.kcal e.fat e.carb
            e.protein e.brand e.name);
      print_newline ())
