import Prelude hiding (lookup)
import qualified TIParser as P
import System.Environment
import Data.Map hiding (map, filter)
import Data.List (intercalate)
import Data.Char (toUpper)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))

data Arity = Exactly Int
  | AtLeast Int
  | Odd
  | Even deriving Show

data FDef = FDef String Arity ((P.Expr -> String) -> [P.Expr] -> String)

builtinDefs = [
            FDef "stop" (Exactly 0) (\_ _ -> "Stop"),
            FDef "clrHome" (Exactly 0) (\_ _ -> "ClrHome"),
            FDef "pause" (Exactly 0) (\_ _ -> "Pause "),
            FDef "prompt" (AtLeast 1) (\c args -> intercalate "\n" (map (buildPrompt . c) args)),
            FDef "rPrompt" (AtLeast 1) (\c args -> "Prompt " ++ (intercalate "," (map c args))),
            FDef "output" (Exactly 3) (\c args -> "Output(" ++ (intercalate "," (map c args)) ++ ")"),
            FDef "eq" (Exactly 2) (\c (a:b:[]) -> (c a)  ++ "=" ++ (c b)),
            FDef "neq" (Exactly 2) (\c (a:b:[]) -> (c a) ++ "≠" ++ (c b)),
            FDef "println" (AtLeast 1) (\c args -> intercalate "\n" (map ((++) "Disp " . c . toStr c) args)),
            FDef "print" (AtLeast 1) (\c args -> "Disp " ++ (intercalate "+" $ map (c . toStr c) args)),
            FDef "sqrt" (Exactly 1) (\c (arg:[]) -> "√(" ++ (c arg) ++ ")"),
            FDef "neg" (Exactly 1) (\c (arg:[]) -> "-" ++ (c arg)),
            FDef "sin" (Exactly 1) (\c (arg:[]) -> "sin(" ++ (c arg) ++ ")"),
            FDef "cos" (Exactly 1) (\c (arg:[]) -> "cos(" ++ (c arg) ++ ")"),
            FDef "tan" (Exactly 1) (\c (arg:[]) -> "tan(" ++ (c arg) ++ ")"),
            FDef "asin" (Exactly 1) (\c (arg:[]) -> "sin\61445(" ++ (c arg) ++ ")"),
            FDef "acos" (Exactly 1) (\c (arg:[]) -> "cos\61445(" ++ (c arg) ++ ")"),
            FDef "atan" (Exactly 1) (\c (arg:[]) -> "tan\61445(" ++ (c arg) ++ ")"),
            FDef "menu" Odd (\c ((P.Str title):args) -> "Menu(\"" ++ title ++ "\"," ++ (intercalate "," . (map $ compMenu c) $ args) ++ ")"),
            FDef "str" (AtLeast 1) (\c args -> intercalate "+" $ map (c . toStr c) args),
            FDef "numAt" (Exactly 2) (\c (name:num:[]) -> (c name) ++ "(" ++ c num ++ ")"),
            FDef "backupVars" (Exactly 1)
              (\c ((P.DynVar lname):[]) ->
                c (P.Store
                  (P.ListLit (map P.Var ('θ':['A'..'Z'])))
                  (P.DynVar lname))),
            FDef "restoreVars" (Exactly 1)
              (\c ((P.DynVar lname):[]) ->
                c . P.Block $
                (map (\(n,v) -> P.Store
                  (P.Raw $
                    (c $ P.DynVar lname) ++ "(" ++ show n ++ ")")
                  (P.Var v))
                (zip [1..27] ('θ':['A'..'Z'])))),
            FDef "delVar" (Exactly 1) (\c (v:[]) -> "DelVar " ++ (c v)),
            FDef "regVars" (Exactly 0) (\c [] -> c (P.ListLit (map P.Var ('θ':['A'..'Z'])))),
            FDef "dim" (Exactly 1) (\c (lName:[]) -> "dim(" ++ (c lName) ++ ")"),
            FDef "setUpEditor" (Exactly 0) (\c [] -> "SetUpEditor "),
            FDef "sum" (Exactly 1) (\c (lName:[]) -> "sum(" ++ (c lName) ++ ")")]

buildPrompt x = "Prompt θ\nθ→" ++ x

toStr :: (P.Expr -> String) -> P.Expr -> P.Expr
toStr _ (P.Str x) = (P.Str x)
toStr c a = P.Raw $ "toString(" ++ (c a) ++ ")"

compMenu :: (P.Expr -> String) -> P.Expr -> String
compMenu c (P.DynVar s) = c (P.RawLabel s)
compMenu c d = (c d)

builtins = fromList $ zipWith (\fd (FDef name _ _) -> (name, fd)) builtinDefs builtinDefs

type Name = String

initBindings = empty

letReduce :: (Int, Map Name P.Expr) -> [P.Expr] -> [P.Expr] -> ((Int, Map Name P.Expr), [P.Expr])
letReduce (i,b) ((P.Let name e):rest) new = letReduce (i, (insert name e b)) rest new
letReduce (i,b) ((P.VarBind name list@(P.ListLit e)):rest) new =
  let lname = "⌊" ++ (map toUpper name)
      varbind = P.Store list (P.Raw lname)
  in  letReduce (i,(insert name (P.Raw lname) b)) rest (varbind:new)
letReduce (i,b) ((P.VarBind name e):rest) new =
  let listVar = P.Raw $ "⌊VARS(" ++ show i ++ ")"
  in letReduce (i + 1, (insert name listVar b)) rest ((P.Store e listVar):new)
letReduce (i,b) ((P.Block exprs):rest) new = let ((blockVars, blockB), blockR) = letReduce (i,b) exprs []
                                      in letReduce (i + blockVars,(b `union` blockB)) rest ((P.Block blockR):new)
letReduce info (x:xs) new = letReduce info xs (x:new)
letReduce info [] new = (info,new)

getLabelList :: [P.Expr] -> [Name]
getLabelList ((P.Label n):rest) = n:(getLabelList rest)
getLabelList (_:rest) = getLabelList rest
getLabelList [] = []

buildLabelMap :: [Name] -> [P.Expr] -> Map Name Name
buildLabelMap basicNames exprs = let labels = getLabelList exprs
                                 in  fromList $ zip labels basicNames

comp :: (Map Name P.Expr, Map Name Name, Map Name FDef) -> P.Expr -> String
comp _ (P.Raw s) = s
comp _ (P.Str s) = '\"' : (s ++ "\"")
comp b (P.Store (P.ListLit []) v) = "SetUpEditor " ++ (comp b v)
comp b (P.Store e v) = (comp b e) ++ "→" ++ (comp b v)
comp (_, labels, _) (P.Goto s) = case lookup s labels of
  Just label -> "Goto " ++ label
  Nothing -> error $ "label " ++ s ++ " not found"
comp (_, labels, _) (P.RawLabel s) = case lookup s labels of
  Just label -> label
  Nothing -> error $ "label " ++ s ++ " not found"
comp b (P.Block es) = intercalate "\n" $ map (comp b) es
comp b (P.Math es) = "(" ++ (intercalate "" $ map (comp b) es) ++ ")"
comp (_, labels, _) (P.Label name) = case lookup name labels of
  Just label -> "Lbl " ++ label
  Nothing -> error $ "Bad def for label " ++ name ++ "\nLabels must be defined in global scope"
comp _ (P.Var x) = x:""
comp _ (P.Nat x) = x
comp _ (P.Op x) = x:""
comp _ (P.Let _ _) = error "Attempt to compile let form"
comp b@(bindings,_,_) (P.DynVar n) = case lookup n bindings of
  Just e -> comp b e --error $ "Attempt to compile dyn form " ++ n
  Nothing -> error $ "binding " ++ n ++ " not found"
comp b (P.If c e1 e2) = case e2 of
  P.Raw "" -> "If " ++ (comp b c) ++ "\nThen\n" ++ (comp b e1) ++ "\nEnd"
  _ -> "If " ++ (comp b c) ++ "\nThen\n" ++ (comp b e1) ++ "\nElse\n" ++ (comp b e2) ++ "\nEnd"
comp b@(_,_,fd) (P.FCall n es) = case lookup n fd of
  Just (FDef _ arity fComp) -> case arity of
    Exactly a | length es == a -> fComp (comp b) es
    AtLeast a | length es >= a -> fComp (comp b) es
    Odd       | odd $ length es -> fComp (comp b) es
  Nothing -> error $ "function " ++ n ++ " not found"
comp b (P.ListLit exprs@(_:_)) = "{" ++ (intercalate "," $ map (comp b) exprs) ++ "}"
comp b (P.ListLit emptyList) = "So there's bad stuff with empty lists"
  -- n ++ "(" ++ (intercalate ", " $ map (comp b) es) ++ ")"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

basicLabelNames :: [String]
basicLabelNames = (map cToS ['A'..'Z']) ++ ((map (++) basicLabelNames) <*> (map cToS ['A'..'Z']))

cToS x = x : ""

-- swapLabels :: Map Name Name -> [P.Expr] -> [P.Expr]
-- swapLabels labelNames = map (swapLabel labelNames)
--
-- swapLabel labelNames (P.Label old) = let Just newName = lookup old labelNames in P.Label newName
-- swapLabel labelNames (P.Goto old) = let Just newName = lookup old labelNames in P.Goto newName
-- swapLabel _ x = x

-- compileString :: String -> Either
compileString str =
  case P.tiParse str of
    Left e -> Left e
    Right v ->
      let ((numVars, bindings), block) = letReduce (1, initBindings) v []
          labelMap = buildLabelMap basicLabelNames block
      in  if numVars == 1
        then Right $ comp (bindings, labelMap, builtins) . P.Block $ reverse block
        else Right $ comp (bindings, labelMap, builtins) . P.Block $ (P.Store (P.Raw $ "seq(1,X,1,"++ show (numVars - 1) ++ ")") (P.Raw "⌊VARS")):(reverse block)

data CompArgs = CompArgs {input :: String
                , output :: String
                , watch :: Bool
                , writeToFile :: Bool} deriving Show

compileFile fname = do
  s <- readFile fname
  case compileString s of
    Right v -> return v
    Left e -> error (show e) >> return ""

defaultArgs = CompArgs {input="", output="", watch=False, writeToFile=False}

parseArgs :: [String] -> CompArgs -> CompArgs
parseArgs ("-f":fname:args) c = parseArgs args (c {input=fname})
parseArgs ("-o":fname:args) c = parseArgs args (c {output=fname, writeToFile=True})
parseArgs ("-w":args) c = parseArgs args (c {watch=True})
parseArgs (_:args) c = parseArgs args c
parseArgs [] c = c

fixTheta :: Char -> Char
fixTheta '\920' = '\952'
fixTheta x = x

main = do
  args <- getArgs
  dir <- getCurrentDirectory
  let settings = parseArgs args defaultArgs
  str <- case input settings of
    "" -> getLine
    fname -> readFile fname
  case compileString str of
    Left e -> putStrLn $ show e
    Right v -> let fixed = map fixTheta v in
      case writeToFile settings of
        True -> writeFile (dir </> (output settings)) fixed
        False -> putStrLn fixed
