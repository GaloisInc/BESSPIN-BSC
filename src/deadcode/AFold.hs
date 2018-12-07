module AFold(aFold) where
import ASyntax
import ASyntaxUtil
-- import Prim
-- import AUses
import Flags(Flags, optUndet)
-- import SMonad
-- import FStringCompat(mkFString)
-- import Position(noPosition)
-- import Id
-- import Util
import ErrorUtil(internalError)
import qualified OrdMap as M
import IntLit

-- import Trace
-- import PPrint

-- constant folding given _ being chosen to be 0 (i.e. optUndet = False)
aFold :: Flags -> APackage -> APackage
aFold flags apkg = if (optUndet flags) then apkg else aFold' apkg  

aFold' :: APackage -> APackage
aFold' apkg = apkg { apkg_state_instances = state',
                     apkg_local_defs = userDefs',
                     apkg_rules = userARules',
                     apkg_interface = ifcs' }
    where (subst, userDefs') = foldDefs (tsortADefs (apkg_local_defs apkg))
          state' = aSubst subst (apkg_state_instances apkg)
          userARules' = aSubst subst (apkg_rules apkg)
          ifcs' = aSubst subst (apkg_interface apkg)

foldDefs :: [ADef] -> (EMap AExpr, [ADef])
foldDefs defs = foldDefs' defs M.empty []

-- really can only "do" primOps for integer literals
isInt :: AExpr -> Bool
isInt (ASInt _ _ _) = True
isInt _ = False

getInt (ASInt _ _ i) = ilValue i
-- should never happen - we checked that they were all ASInts first
getInt _ = internalError("getInt")

getWidth (ASInt _ (ATBit w) _) = w
-- should never happen - all integer literals should have type ATBit
getWidth _ = internalError("getWidth")

foldDefs' :: [ADef] -> EMap AExpr -> [ADef] -> (EMap AExpr, [ADef])
foldDefs' [] subst defs = (subst, (reverse defs))
foldDefs' (d:ds) subst rest =
  let d'@(ADef i t e')  = aSubst subst d in
      case e' of 
            -- substitute in known constants
           es@(ASInt _ _ _) -> foldDefs' ds (M.add (i, es) subst) (d':rest)

           (APrim _ t' op es) | (all isInt es) -> 
              let (ATBit reswidth) = t'
                  eints = map getInt es
                  ewidths = map getWidth es
                  aResult = doPrimOp op (reswidth:ewidths) eints 
              in
                case aResult of
                -- weren't able to "do" the (or didn't get a constant back)
                     Nothing    -> {- trace ("doPrimOp Nothing: " ++ (ppReadable e') ++ "\n") $ -} foldDefs' ds subst (d':rest)
                     (Just e'') -> foldDefs' ds (M.add (i, e'') subst) ((ADef i t e''):rest)
  
           _ -> foldDefs' ds subst (d':rest)
