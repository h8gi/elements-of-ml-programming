(* 1次元セルオートマトンの実装 *)

(* ルールの計算関数 *)
fun applyRule (rule: int) (left: bool) (center: bool) (right: bool): bool =
  let
    val index = (if left then 4 else 0) +
                (if center then 2 else 0) +
                (if right then 1 else 0)
    val bitMask = Word.fromInt(1) << Word.fromInt(index)
    val result = Word.andb(Word.fromInt(rule), bitMask)
  in
    not (Word.toInt(result) = 0)
  end;

(* セルの状態を更新する関数 *)
fun nextGeneration rule currentGen =
  let
    val len = List.length currentGen

    (* 左右の境界処理を含む隣接セル取得 *)
    fun getCell i =
      if i < 0 then List.nth(currentGen, len-1)
      else if i >= len then List.nth(currentGen, 0)
      else List.nth(currentGen, i)

    (* 各セルに対してルールを適用 *)
    fun processCell i =
      if i >= len then []
      else
        let
          val left = getCell(i-1)
          val center = getCell(i)
          val right = getCell(i+1)
        in
          applyRule rule left center right :: processCell (i+1)
        end
  in
    processCell 0
  end;

(* 初期状態の生成 *)
fun initialState size =
  let
    fun init i acc =
      if i = 0 then acc
      else init (i-1) (false :: acc)

    val state = init size []

    (* 中央のセルだけをtrueにする *)
    val midIndex = size div 2

    fun setMiddle (xs, i) =
      if i = 0 then
        List.hd xs :: List.tl xs
      else
        List.hd xs :: setMiddle(List.tl xs, i-1)
  in
    List.take(state, midIndex) @ [true] @ List.drop(state, midIndex+1)
  end;

(* セルオートマトンの実行 *)
fun runCA rule size generations =
  let
    val initialGen = initialState size

    fun iterate gen count acc =
      if count = 0 then List.rev(gen :: acc)
      else iterate (nextGeneration rule gen) (count-1) (gen :: acc)
  in
    iterate initialGen generations []
  end;

(* 描画用の文字列変換 *)
fun printGeneration gen =
  let
    fun cellToChar cell = if cell then "#" else " "

    fun printCells [] = ""
      | printCells (cell::rest) = cellToChar cell ^ printCells rest
  in
    printCells gen
  end;

(* 全世代を描画 *)
fun printAllGenerations generations =
  let
    fun printGens [] = ()
      | printGens (gen::rest) =
          (print(printGeneration gen ^ "\n"); printGens rest)
  in
    printGens generations
  end;

(* メイン関数 *)
fun main() =
  let
    val rule = 30     (* ルール30を使用 *)
    val size = 79     (* 画面幅に合わせた幅 *)
    val gens = 40     (* 表示する世代数 *)

    val allGenerations = runCA rule size gens
  in
    print("セルオートマトン ルール" ^ Int.toString(rule) ^ ":\n");
    printAllGenerations allGenerations
  end;

(* プログラムを実行 *)
main();
