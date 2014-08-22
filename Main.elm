import Graphics.Input (input, Input, customButton)
import Maybe (..)
import Either (..)
import Json (..)
import Text
data Tag = Error String 

data LambdaTerm = Var String 
                | App LambdaTerm LambdaTerm 
                | Lambda String LambdaTerm
                | Tagged Tag LambdaTerm

data Instruction = BetaReduce
                 | EtaContract
                 | AlphaRename 
                 | InLambda Instruction
                 | InAppL Instruction
                 | InAppR Instruction
                 | ClearTag
                 | ResetTerm Value

data Rewrite = Initial | Alpha | Beta | Eta

toTerm : Value -> LambdaTerm
toTerm x = case x of 
  Array [String "LAMBDA", String x, t ] -> Lambda x (toTerm t)
  Array [String "APP", a, b ] -> App (toTerm a) (toTerm b)
  String x -> Var x

state : Value -> (Bool, [(Rewrite, LambdaTerm)])
state v = (False, [(Initial, toTerm v)])

port termsIn : Signal Value

rewrite : Instruction -> Maybe Rewrite
rewrite i = case i of
  BetaReduce      -> Just Beta
  EtaContract     -> Just Eta
  AlphaRename     -> Just Alpha
  (InLambda i)    -> rewrite i
  (InAppL i)      -> rewrite i
  (InAppR i)      -> rewrite i
  _               -> Nothing

stripTags : LambdaTerm -> LambdaTerm
stripTags x = case x of
  (Tagged s t) -> stripTags t
  (Lambda y t) -> Lambda y (stripTags t)
  (App a b)    -> App (stripTags a) (stripTags b)
  _            -> x

updateTerms : [(Rewrite, LambdaTerm)] 
            -> (Int,Instruction)
            -> [(Rewrite, LambdaTerm)]
updateTerms l (n, i) = case (l, n) of
  ((rw, t) :: ts, 0) -> case (updateTerm t i, rewrite i) of 
     ((t',True),Just rw') -> (rw,stripTags t) :: [(rw',t')]
     ((t',False),_)       -> [(rw,t')]
  (t :: ts, n) -> t :: updateTerms ts (n-1, i)

occurs : String -> LambdaTerm -> Bool
occurs x term = case term of
  (Tagged s t) -> occurs x t
  (Lambda y t) -> if x == y then False else occurs x t
  (App a b)    -> occurs x a || occurs x b
  (Var y)      -> x == y

(>>=) : Either e a -> (a -> Either e b) -> Either e b
t >>= f = case t of Right a -> f a
                    Left b -> Left b
                    

substitute : String -> LambdaTerm -> LambdaTerm -> Either String LambdaTerm
substitute x t term = case term of
  (Var y)      -> if x == y then Right t else Right (Var y)
  (App a b)    -> substitute x t a >>= \a' ->
                    substitute x t b >>= \b' ->
                      Right (App a' b')
  (Lambda y b) -> if x == y 
                    then Right (Lambda y b)
                    else if occurs y t then Left y
                                       else either Left (Right . Lambda y) (substitute x t b)
  (Tagged s b) -> either (Left) (Right . Tagged s) (substitute x t b)


findRenaming : String -> String -> LambdaTerm -> (String, LambdaTerm)
findRenaming x x' t = case substitute x (Var (x' ++ "'")) t of
                         Left e -> findRenaming x (x' ++ "'") t
                         Right b -> (x' ++ "'", b)

updateTerm : LambdaTerm -> Instruction -> (LambdaTerm, Bool)
updateTerm term inst = case (term,inst) of
  (Tagged s t, ClearTag) -> (t, False)
  (Tagged s t, i)        -> updateTerm t i
  (App (Lambda x t) a, BetaReduce)
    -> case substitute x a t of
         Left e -> ( Tagged (Error (e ++ " bound"))
                           (App (Lambda x t) a)
                   , False)
         Right t' -> (t', True)
  (Lambda x (App e (Var y)), EtaContract)
    -> (e, True) -- assuming x == y, which should be true from input
  (Lambda x t, AlphaRename) 
    -> let (x',t') = (findRenaming x x t) in (Lambda x' t', True)
  (Lambda x t, InLambda i) 
    -> let (t',b) = updateTerm t i in (Lambda x t', b)
  (App a b, InAppL i)
    -> let (a',s) = updateTerm a i in (App a' b,s)
  (App a b, InAppR i)
    -> let (b',s) = updateTerm b i in (App a b',s)



instructions : Input (Maybe (Int, Instruction))
instructions = input Nothing


updateState : (Bool, [(Rewrite, LambdaTerm)] )
            -> Maybe (Int,Instruction)
            -> (Bool, [(Rewrite, LambdaTerm)])
updateState (b,s) = maybe (False,s) (\(i,is) -> case is of
              ResetTerm v  -> state v
              _            -> (True, if b then s else updateTerms s (i,is)))

initialState = (False, [(Initial,
                  App (Lambda "n" (Lambda "f" (Lambda "x" 
                          (App (Var "f") ((App (App (Var "n") (Var "x")) (Var "x")))))))
                      (Lambda "f" (Lambda "x" (Var "x"))))])


displayRewrite : Rewrite -> Element
displayRewrite rw = case rw of
  Initial -> plainText' "  → "
  Alpha   -> plainText' "=α= "
  Eta     -> plainText' "-η→ "
  Beta    -> plainText' "-β→ "

displayTerm : Int -> (Instruction -> Instruction) -> [String] -> LambdaTerm -> Element
displayTerm i f bvs term = case term of 
  (App (Lambda x t) a) -> let raw = displayTermRaw i f bvs term 
                           in customButton (instructions.handle) (Just <| (\x -> (i,x)) <| f (BetaReduce))
                              raw (color lightYellow raw) (color yellow raw)
  (Lambda x (App t (Var x'))) -> let raw = displayTermRaw i f bvs term 
    in if x == x' then customButton (instructions.handle) (Just <| (\x -> (i,x)) <| f (EtaContract))
                              raw (color lightOrange raw) (color orange raw)
                  else raw
  _ -> displayTermRaw i f bvs term


textStyle : Text.Style
textStyle = { typeface = [ "PragmataPro", "Droid Sans", "Envy Code R", "Inconsolata", "Consolas", "monospace" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing
  }

plainText' = leftAligned . Text.style textStyle . toText
displayTag : Int -> (Instruction -> Instruction) -> Tag -> Element
displayTag i f tag = case tag of 
  (Error str) -> color lightRed (flow right 
                   [ plainText str
                   , customButton (instructions.handle) (Just <| (\x -> (i,x)) <| f (ClearTag)) 
                                  (color red (plainText "☑")) (color orange (plainText "☑")) (plainText "☑")])
  _ -> plainText "unimplemented"

displayTermRaw : Int -> (Instruction -> Instruction) -> [String] -> LambdaTerm -> Element
displayTermRaw i f bvs term = case term of 
  (Tagged g t) -> flow down [displayTerm i f bvs t, displayTag i f g]
  (Lambda x t) -> let lambdaButton = plainText' ("λ" ++ x ++".")
                   in flow right [ customButton (instructions.handle) (Just <| (\x -> (i,x)) <| f (AlphaRename)) 
                                                lambdaButton (color lightBlue lambdaButton) (color blue lambdaButton)
                             , plainText' " ", displayTerm i (f . InLambda) (x :: bvs) t ]
  (Var x) -> if all (\y -> x /= y) bvs then leftAligned <| bold <| Text.style textStyle <| toText x else plainText' x  
  (App a b) -> flow right [displayTerm'' i (f . InAppL) bvs a, plainText' " ", displayTerm' i (f . InAppR) bvs b]

displayTerm'' : Int -> (Instruction -> Instruction) -> [String] -> LambdaTerm -> Element
displayTerm'' i f bvs term = case term of 
  (Lambda _ _) -> flow right [plainText' "(", displayTerm i f bvs term, plainText' ")"]
  (Tagged _ (Lambda _ _)) -> flow right [plainText' "(", displayTerm i f bvs term, plainText' ")"]
  _            -> displayTerm i f bvs term



displayTerm' : Int -> (Instruction -> Instruction) -> [String] -> LambdaTerm -> Element
displayTerm' i f bvs term = case term of 
  (Var _) -> displayTerm i f bvs term
  _       -> flow right [plainText "(", displayTerm i f bvs term, plainText ")"]


displayReduction : Int -> (Rewrite, LambdaTerm) -> Element
displayReduction i (rw, t) = flow right [displayRewrite rw, displayTerm i (\x -> x) [] t]
displayState : (Bool, [(Rewrite, LambdaTerm)]) -> Element
displayState (_,rs) = flow down (intersperse (spacer 5 10) (zipWith displayReduction [0 .. length rs] rs))

currentState : Signal (Bool, [(Rewrite, LambdaTerm)])
currentState = foldp (flip updateState) initialState <|
                 merges [ instructions.signal
                        , always Nothing <~ every second 
                        , (\x -> Just (0,ResetTerm x)) <~ termsIn ]

main : Signal Element
main = displayState <~ currentState
