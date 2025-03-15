use "hash_table.sml"; (* hash_table.sml を読み込む *)

CM.make "sources.cm";
val m = SiMapping.create;
val m = SiMapping.insert("in", 6, m);
val v = SiMapping.lookup("in", m);

val t1 = Tree.build(20, [Tree.create(3), Tree.create(4)]);
val t2 = Tree.find(1, t1);

(* exercise 8.2.3 *)
val t3 = SimpleTree.build(1, [
			     SimpleTree.build(2, nil),
			     SimpleTree.build(3, nil),
			     SimpleTree.build(4, nil)
			 ]);
val t4 = SimpleTree.find(1, t3);

val bst1 = StringBST.create;

(* MisspellSet の動作確認 *)
val misspellSet = MisspellSet.create;
val misspellSet = MisspellSet.insert("hello", misspellSet);
val misspellSet = MisspellSet.insert("hallo", misspellSet);
val similarWords = MisspellSet.findSim("helli", misspellSet);

(* Hash100 の動作確認 *)
val hash_table = Hash100.create (); (* ハッシュテーブルの作成 *)

(* insert の動作確認 *)
val _ = Hash100.insert ("apple", hash_table);
val _ = Hash100.insert ("banana", hash_table);
val _ = Hash100.insert ("cherry", hash_table);

(* lookup の動作確認 *)
val has_apple = Hash100.lookup ("apple", hash_table); (* true が返るはず *)
val has_grape = Hash100.lookup ("grape", hash_table); (* false が返るはず *)

(* delete の動作確認 *)
val _ = Hash100.delete ("banana", hash_table);
val has_banana = Hash100.lookup ("banana", hash_table); (* false が返るはず *)
val has_cherry = Hash100.lookup ("cherry", hash_table); (* true が返るはず *)

print ("has_apple: " ^ Bool.toString(has_apple) ^ "\n");
print ("has_grape: " ^ Bool.toString(has_grape) ^ "\n");
print ("has_banana: " ^ Bool.toString(has_banana) ^ "\n");
print ("has_cherry: " ^ Bool.toString(has_cherry) ^ "\n");
