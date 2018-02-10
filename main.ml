(* Open libraries *)
module DB_Stats = Database_stats;;
open DB_Stats;;

(* Get stats *)
let _ = DB_Stats.image_results ();;
