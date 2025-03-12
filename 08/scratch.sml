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
val similarWords = MisspellSet.findSim("helo", misspellSet);
