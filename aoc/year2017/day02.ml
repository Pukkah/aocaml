let input' = "5 1 9 5\n7 5 3\n2 4 6 8"

let input =
  "414\t382\t1515\t319\t83\t1327\t116\t391\t101\t749\t1388\t1046\t1427\t105\t1341\t1590\n\
   960\t930\t192\t147\t932\t621\t1139\t198\t865\t820\t597\t165\t232\t417\t19\t183\n\
   3379\t987\t190\t3844\t1245\t1503\t3151\t3349\t2844\t4033\t175\t3625\t3565\t179\t3938\t184\n\
   116\t51\t32\t155\t102\t92\t65\t42\t48\t91\t74\t69\t52\t89\t20\t143\n\
   221\t781\t819\t121\t821\t839\t95\t117\t626\t127\t559\t803\t779\t543\t44\t369\n\
   199\t2556\t93\t1101\t122\t124\t2714\t625\t2432\t1839\t2700\t2636\t118\t2306\t1616\t2799\n\
   56\t804\t52\t881\t1409\t47\t246\t1368\t1371\t583\t49\t1352\t976\t400\t1276\t1240\n\
   1189\t73\t148\t1089\t93\t76\t3205\t3440\t3627\t92\t853\t95\t3314\t3551\t2929\t3626\n\
   702\t169\t492\t167\t712\t488\t357\t414\t187\t278\t87\t150\t19\t818\t178\t686\n\
   140\t2220\t1961\t1014\t2204\t2173\t1513\t2225\t443\t123\t148\t580\t833\t1473\t137\t245\n\
   662\t213\t1234\t199\t1353\t1358\t1408\t235\t917\t1395\t1347\t194\t565\t179\t768\t650\n\
   119\t137\t1908\t1324\t1085\t661\t1557\t1661\t1828\t1865\t432\t110\t658\t821\t1740\t145\n\
   1594\t222\t4140\t963\t209\t2782\t180\t2591\t4390\t244\t4328\t3748\t4535\t192\t157\t3817\n\
   334\t275\t395\t128\t347\t118\t353\t281\t430\t156\t312\t386\t160\t194\t63\t141\n\
   146\t1116\t153\t815\t2212\t2070\t599\t3018\t2640\t47\t125\t2292\t165\t2348\t2694\t184\n\
   1704\t2194\t1753\t146\t2063\t1668\t1280\t615\t163\t190\t2269\t1856\t150\t158\t2250\t2459"
;;

let parse input =
  Base.String.substr_replace_all ~pattern:"\t" ~with_:" " input
  |> Util.get_lines
  |> List.map Util.parse_numbers
;;

let row_dif row =
  let min', max' =
    List.fold_left (fun (min', max') x -> min min' x, max max' x) (max_int, min_int) row
  in
  max' - min'
;;

let checksum rows = List.fold_left ( + ) 0 (List.map row_dif rows)

let div row =
  let rec aux = function
    | [] -> 0
    | x :: xs ->
      (match List.find_opt (fun y -> x mod y = 0 || y mod x = 0) xs with
       | Some y -> if x mod y = 0 then x / y else y / x
       | None -> aux xs)
  in
  aux row
;;

let run () =
  parse input |> checksum |> print_int;
  print_newline ();
  parse input |> List.map div |> List.fold_left ( + ) 0 |> print_int;
  print_newline ()
;;
