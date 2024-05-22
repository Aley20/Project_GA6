# Exécutables
BASIQUE_EXEC = latsi_basique
EX1_EXEC = latsi_ex1
EX2_EXEC = latsi_ex2
EX3_EXEC = latsi_ex3

# Répertoires source
BASIQUE_DIR = versions/basique
EXT1_DIR = versions/ex1
EXT2_DIR = versions/ex2
EXT3_DIR = versions/ex3

# Répertoires de génération
GENERATED_DIR = fichier_generer

# Répertoires de tests
TEST_BASIQUE_DIR = tests/basique
TEST_EXT1_DIR = tests/ex1
TEST_EXT2_DIR = tests/ex2
TEST_EXT3_DIR = tests/ex3

# Fichiers spécifiques
BASIQUE_FILES = $(BASIQUE_DIR)/ast.ml $(BASIQUE_DIR)/lexer.mll $(BASIQUE_DIR)/parser.mly $(BASIQUE_DIR)/interpreter.ml
EXT1_FILES = $(EXT1_DIR)/ast.ml $(EXT1_DIR)/lexer.mll $(EXT1_DIR)/parser.mly $(EXT1_DIR)/interpreter.ml
EXT2_FILES = $(EXT2_DIR)/ast.ml $(EXT2_DIR)/lexer.mll $(EXT2_DIR)/parser.mly $(EXT2_DIR)/interpreter.ml
EXT3_FILES = $(EXT3_DIR)/ast.ml $(EXT3_DIR)/lexer.mll $(EXT3_DIR)/parser.mly $(EXT3_DIR)/interpreter.ml

# Cibles par défaut
all: $(BASIQUE_EXEC) $(EX1_EXEC) $(EX2_EXEC) $(EX3_EXEC) 

# Compilation de la version basique
$(BASIQUE_EXEC): $(BASIQUE_FILES)
	mkdir -p $(GENERATED_DIR)
	ocamlc -c -I $(GENERATED_DIR) $(BASIQUE_DIR)/ast.ml -o $(GENERATED_DIR)/ast.cmo
	menhir --ocamlc "ocamlc -I $(GENERATED_DIR) -I $(BASIQUE_DIR)" --infer --base $(GENERATED_DIR)/parser $(BASIQUE_DIR)/parser.mly
	ocamllex -o $(GENERATED_DIR)/lexer.ml $(BASIQUE_DIR)/lexer.mll
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.mli
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.ml
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/lexer.ml
	ocamlc -c -I $(GENERATED_DIR) $(BASIQUE_DIR)/interpreter.ml
	ocamlc -o $(BASIQUE_EXEC) $(GENERATED_DIR)/ast.cmo $(GENERATED_DIR)/parser.cmo $(GENERATED_DIR)/lexer.cmo $(BASIQUE_DIR)/interpreter.cmo

# Compilation de la version avec l'extension 1
$(EX1_EXEC): $(EXT1_FILES)
	mkdir -p $(GENERATED_DIR)
	ocamlc -c -I $(GENERATED_DIR) $(EXT1_DIR)/ast.ml -o $(GENERATED_DIR)/ast.cmo
	menhir --ocamlc "ocamlc -I $(GENERATED_DIR) -I $(EXT1_DIR)" --infer --base $(GENERATED_DIR)/parser $(EXT1_DIR)/parser.mly
	ocamllex -o $(GENERATED_DIR)/lexer.ml $(EXT1_DIR)/lexer.mll
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.mli
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.ml
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/lexer.ml
	ocamlc -c -I $(GENERATED_DIR) $(EXT1_DIR)/interpreter.ml
	ocamlc -o $(EX1_EXEC) $(GENERATED_DIR)/ast.cmo $(GENERATED_DIR)/parser.cmo $(GENERATED_DIR)/lexer.cmo $(EXT1_DIR)/interpreter.cmo

# Compilation de la version avec l'extension 2
$(EX2_EXEC): $(EXT2_FILES)
	mkdir -p $(GENERATED_DIR)
	ocamlc -c -I $(GENERATED_DIR) $(EXT2_DIR)/ast.ml -o $(GENERATED_DIR)/ast.cmo
	menhir --ocamlc "ocamlc -I $(GENERATED_DIR) -I $(EXT2_DIR)" --infer --base $(GENERATED_DIR)/parser $(EXT2_DIR)/parser.mly
	ocamllex -o $(GENERATED_DIR)/lexer.ml $(EXT2_DIR)/lexer.mll
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.mli
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.ml
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/lexer.ml
	ocamlc -c -I $(GENERATED_DIR) $(EXT2_DIR)/interpreter.ml
	ocamlc -o $(EX2_EXEC) $(GENERATED_DIR)/ast.cmo $(GENERATED_DIR)/parser.cmo $(GENERATED_DIR)/lexer.cmo $(EXT2_DIR)/interpreter.cmo

# Compilation de la version avec l'extension 3
$(EX3_EXEC): $(EXT3_FILES)
	mkdir -p $(GENERATED_DIR)
	ocamlc -c -I $(GENERATED_DIR) $(EXT3_DIR)/ast.ml -o $(GENERATED_DIR)/ast.cmo
	menhir --ocamlc "ocamlc -I $(GENERATED_DIR) -I $(EXT3_DIR)" --infer --base $(GENERATED_DIR)/parser $(EXT3_DIR)/parser.mly
	ocamllex -o $(GENERATED_DIR)/lexer.ml $(EXT3_DIR)/lexer.mll
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.mli
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/parser.ml
	ocamlc -c -I $(GENERATED_DIR) $(GENERATED_DIR)/lexer.ml
	ocamlc -c -I $(GENERATED_DIR) $(EXT3_DIR)/interpreter.ml
	ocamlc -o $(EX3_EXEC) $(GENERATED_DIR)/ast.cmo $(GENERATED_DIR)/parser.cmo $(GENERATED_DIR)/lexer.cmo $(EXT3_DIR)/interpreter.cmo

# Nettoyage des fichiers générés
clean:
	ocamlbuild -clean
	rm -f $(BASIQUE_EXEC) $(EX1_EXEC) $(EX2_EXEC) $(EX3_EXEC) $(GENERATED_DIR)/* *~
	rm -f versions/basique/parser.ml versions/basique/parser.mli versions/basique/interpreter.cmi versions/basique/interpreter.cmo
	rm -f versions/basique/ast.cmi versions/basique/ast.cmo versions/basique/lexer.ml
	rm -f versions/ex1/parser.ml versions/ex1/parser.mli versions/ex1/interpreter.cmi versions/ex1/interpreter.cmo
	rm -f versions/ex2/parser.ml versions/ex2/parser.mli versions/ex2/interpreter.cmi versions/ex2/interpreter.cmo
	rm -f versions/ex3/parser.ml versions/ex3/parser.mli versions/ex3/interpreter.cmi versions/ex3/interpreter.cmo
.PHONY: all clean test_basique test_ex1 test_ex2 test_ex3
