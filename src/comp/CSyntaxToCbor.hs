{-# LANGUAGE OverloadedStrings #-}
module CSyntaxToCbor
( cPackageToCbor
, cPackageToCborBytes
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

--import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR

import CSyntax
--import CType
import qualified FStringCompat as FS
import Id
import IntLit
import Position
import Pragma


class ToCbor a where
    toCbor :: a -> CBOR.Term

instance ToCbor Text where
    toCbor t = CBOR.TString t

instance ToCbor Bool where
    toCbor b = CBOR.TBool b

instance ToCbor a => ToCbor (Maybe a) where
    toCbor Nothing = CBOR.TNull
    toCbor (Just x) = toCbor x

instance ToCbor a => ToCbor [a] where
    toCbor xs = CBOR.TList $ map toCbor xs

instance (ToCbor a, ToCbor b) => ToCbor (a, b) where
    toCbor (a, b) = CBOR.TList [toCbor a, toCbor b]

instance ToCbor Int where
    toCbor x = CBOR.TInt x

instance ToCbor Integer where
    toCbor x = CBOR.TInteger x

instance ToCbor Double where
    toCbor x = CBOR.TDouble x


node name parts = CBOR.TList $ CBOR.TString name : parts

instance ToCbor CPackage where
    toCbor (CPackage name _ imports _ defns _) = node "Package"
        [ toCbor name
        , toCbor imports
        , toCbor defns
        ]

instance ToCbor CImport where
    toCbor (CImpId qual i) = node "Import_Id" [ toCbor qual, toCbor i ]
    toCbor (CImpSign _objPath _qual sig) = node "Import_Sign" [ toCbor sig ]

instance ToCbor CSignature where
    toCbor (CSignature name _deps _ _) = node "Signature" [ toCbor name ]

instance ToCbor CDefn where
    toCbor (Ctype i is ty) = node "Defn_Type" [ toCbor i, toCbor is, toCbor ty ]
    toCbor (Cdata _ name tyVars _ _ _) = node "Defn_Data" [ toCbor name, toCbor tyVars ]
    toCbor (Cstruct _ sub name tyVars fields _) = node "Defn_Struct"
        [ toCbor sub, toCbor name, toCbor tyVars, toCbor fields ]
    toCbor (Cclass _ _ name tyVars _ fields) = node "Defn_Class"
        [ toCbor name, toCbor tyVars, toCbor fields ]
    toCbor (Cinstance _ _) = node "Defn_Instance" [ ]
    toCbor (CValue _ _) = node "Defn_Value" [ ]
    toCbor (CValueSign def) = node "Defn_ValueSign" [ toCbor def ]
    toCbor (Cforeign name _ _ _) = node "Defn_Foreign" [ toCbor name ]
    toCbor (Cprimitive name _) = node "Defn_Primitive" [ toCbor name ]
    toCbor (CprimType _) = node "Defn_PrimType" [ ]
    toCbor (CPragma _) = node "Defn_Pragma" [ ]
    toCbor (CIinstance name _) = node "Defn_IInstance" [ toCbor name ]
    toCbor (CItype _ is _) = node "Defn_IType" [ toCbor is ]
    toCbor (CIclass _ _ _ is _ _) = node "Defn_IClass" [ toCbor is ]
    toCbor (CIValueSign i _) = node "Defn_IValueSign" [ toCbor i ]

instance ToCbor CDef where
    toCbor (CDef name ty clauses) = node "Def"
        [ toCbor name, CBOR.TList [], toCbor ty, toCbor clauses ]
    toCbor (CDefT name tyVars ty clauses) = node "Def"
        [ toCbor name, toCbor tyVars, toCbor ty, toCbor clauses ]

instance ToCbor CClause where
    toCbor (CClause pats quals body) = node "Clause"
        [ toCbor pats, toCbor quals, toCbor body ]

instance ToCbor CPat where
    toCbor (CPCon name args) = node "Pat_Con" [ toCbor name, toCbor args ]
    toCbor (CPstruct name _) = node "Pat_Struct" [ toCbor name ]
    toCbor (CPVar i) = node "Pat_Var" [ toCbor i ]
    toCbor (CPAs i p) = node "Pat_As" [ toCbor i, toCbor p ]
    toCbor (CPAny _) = node "Pat_Any" [ ]
    toCbor (CPLit _) = node "Pat_Lit" [ ]
    toCbor (CPMixedLit _ _ _) = node "Pat_MixedLit" [ ]
    toCbor (CPOper _) = node "Pat_Oper" [ ]
    toCbor (CPCon1 conTy i arg) = node "Pat_Con1" [ toCbor conTy, toCbor i, toCbor arg ]
    toCbor (CPConTs conTy i tys args) = node "Pat_ConTs"
        [ toCbor conTy, toCbor i, toCbor tys, toCbor args ]

instance ToCbor CExpr where
    toCbor (CLam (Right i) body) = node "Expr_Lam" [ toCbor i, CBOR.TNull, toCbor body ]
    toCbor (CLam (Left _) body) = node "Expr_Lam" [ CBOR.TNull, CBOR.TNull, toCbor body ]
    toCbor (CLamT (Right i) ty body) = node "Expr_Lam" [ toCbor i, toCbor ty, toCbor body ]
    toCbor (CLamT (Left _) ty body) = node "Expr_Lam" [ CBOR.TNull, toCbor ty, toCbor body ]
    toCbor (Cletseq defls body) = node "Expr_LetSeq" [ toCbor defls, toCbor body ]
    toCbor (Cletrec defls body) = node "Expr_LetRec" [ toCbor defls, toCbor body ]
    toCbor (CSelect e field) = node "Expr_Select" [ toCbor e, toCbor field ]
    toCbor (CCon con args) = node "Expr_Con" [ toCbor con, toCbor args ]
    toCbor (Ccase _ e _) = node "Expr_Case" [ toCbor e ]
    toCbor (CStruct i _) = node "Expr_Struct" [ toCbor i ]
    toCbor (CStructUpd e _) = node "Expr_StructUpd" [ toCbor e ]
    toCbor (Cwrite _ l r) = node "Expr_Write" [ toCbor l, toCbor r ]
    toCbor (CAny _ _) = node "Expr_Any" [ ]
    toCbor (CVar i) = node "Expr_Var" [ toCbor i ]
    toCbor (CApply f args) = node "Expr_Apply" [ toCbor f, toCbor args ]
    toCbor (CTaskApply _ _) = node "Expr_TaskApply" [ ]
    toCbor (CTaskApplyT _ _ _) = node "Expr_TaskApplyT" [ ]
    toCbor (CLit l) = node "Expr_Lit" [ toCbor l ]
    toCbor (CBinOp l op r) = node "Expr_BinOp" [ toCbor l, toCbor op, toCbor r ]
    toCbor (CHasType e ty) = node "Expr_HasType" [ toCbor e, toCbor ty ]
    toCbor (Cif _ c t e) = node "Expr_If" [ toCbor c, toCbor t, toCbor e ]
    toCbor (CSub _ arr idx) = node "Expr_Sub" [ toCbor arr, toCbor idx ]
    toCbor (CSub2 arr idx1 idx2) = node "Expr_SubRange"
        [ toCbor arr, toCbor idx1, toCbor idx2 ]
    toCbor (CSubUpdate _ arr (idx1, idx2) val) = node "Expr_SubRangeUpd"
        [ toCbor arr, toCbor idx1, toCbor idx2, toCbor val ]
    toCbor (Cmodule _ mstmts) = node "Expr_Module" [ toCbor mstmts ]
    toCbor (Cinterface _ optI defls) = node "Expr_Interface" [ toCbor optI, toCbor defls ]
    toCbor (CmoduleVerilog _ _ _ _ _ _ _ _) = node "Expr_ModuleVerilog" [ ]
    toCbor (CForeignFuncC i _) = node "Expr_ForeignFunc" [ toCbor i ]
    toCbor (Cdo rec stmts) = node "Expr_Do" [ toCbor rec, toCbor stmts ]
    toCbor (Caction _ stmts) = node "Expr_Action" [ toCbor stmts ]
    toCbor (Crules _ rs) = node "Expr_Rules" [ toCbor rs ]
    toCbor (CADump es) = node "Expr_ADump" [ toCbor es ]
    toCbor (COper _) = node "Expr_Oper" [ ]
    toCbor (CCon1 ty con e) = node "Expr_Con1" [ toCbor ty, toCbor con, toCbor e ]
    toCbor (CSelectTT ty e field) = node "Expr_SelectTT" [ toCbor ty, toCbor e, toCbor field ]
    toCbor (CCon0 optTy con) = node "Expr_Con0" [ toCbor optTy, toCbor con ]
    toCbor (CConT ty con args) = node "Expr_ConT" [ toCbor ty, toCbor con, toCbor args ]
    toCbor (CStructT ty fields) = node "Expr_StructT" [ toCbor ty, toCbor fields ]
    toCbor (CSelectT ty field) = node "Expr_SelectT" [ toCbor ty, toCbor field ]
    toCbor (CLitT ty l) = node "Expr_LitT" [ toCbor ty, toCbor l ]
    toCbor (CAnyT _ _ ty) = node "Expr_AnyT" [ toCbor ty ]
    toCbor (CmoduleVerilogT _ _ _ _ _ _ _ _ _) = node "Expr_ModuleVerilogT" [ ]
    toCbor (CForeignFuncCT id ty) = node "Expr_ForeignFuncT" [ toCbor id, toCbor ty ]
    toCbor (CTApply e tys) = node "Expr_TyApply" [ toCbor e, toCbor tys ]
    toCbor (Cattributes _) = node "Expr_Attributes" [ ]

instance ToCbor CRule where
    toCbor (CRule _ nameExpr quals body) = node "Rule"
        [ toCbor nameExpr, toCbor quals, toCbor body ]
    toCbor (CRuleNest _ nameExpr quals rules) = node "Rule_Nest"
        [ toCbor nameExpr, toCbor quals, toCbor rules ]

instance ToCbor CQual where
    toCbor (CQGen ty p e) = node "Qual_Gen" [ toCbor ty, toCbor p, toCbor e ]
    toCbor (CQFilter e) = node "Qual_Filter" [ toCbor e ]

instance ToCbor CLiteral where
    toCbor (CLiteral pos (LString s)) = node "Lit_Str" [ toCbor pos, toCbor $ T.pack s ]
    toCbor (CLiteral pos (LChar c)) = node "Lit_Char" [ toCbor pos, toCbor $ T.singleton c ]
    toCbor (CLiteral pos (LInt (IntLit width base value))) =
        node "Lit_Int" [ toCbor pos, toCbor width, toCbor base, toCbor value ]
    toCbor (CLiteral pos (LReal x)) = node "Lit_Real" [ toCbor pos, toCbor x ]
    toCbor (CLiteral pos LPosition) = node "Lit_Position" [ toCbor pos ]

instance ToCbor CDefl where
    toCbor (CLValueSign def _) = node "Defl_ValueSign" [ toCbor def ]
    toCbor (CLValue i clauses _) = node "Defl_Value" [ toCbor i, toCbor clauses ]
    toCbor (CLMatch p e) = node "Defl_Match" [ toCbor p, toCbor e ]

instance ToCbor CStmt where
    toCbor (CSBindT p _ _ ty e) = node "Stmt_Bind" [ toCbor p, toCbor ty, toCbor e ]
    toCbor (CSBind p _ _ e) = node "Stmt_Bind" [ toCbor p, CBOR.TNull, toCbor e ]
    toCbor (CSletseq defls) = node "Stmt_LetSeq" [ toCbor defls ]
    toCbor (CSletrec defls) = node "Stmt_LetRec" [ toCbor defls ]
    toCbor (CSExpr _ e) = node "Stmt_Expr" [ toCbor e ]

instance ToCbor CMStmt where
    toCbor (CMStmt s) = node "MStmt_Stmt" [ toCbor s ]
    toCbor (CMrules e) = node "MStmt_Rules" [ toCbor e ]
    toCbor (CMinterface e) = node "MStmt_Interface" [ toCbor e ]
    toCbor (CMTupleInterface _ es) = node "MStmt_TupleInterface" [ toCbor es ]

instance ToCbor CField where
    toCbor (CField name pragmas ty _ _) =
        node "Field" [ toCbor name, toCbor pragmas, toCbor ty ]

instance ToCbor IfcPragma where
    toCbor (PIArgNames is) = node "IfcPragma_ArgNames" [ toCbor is ]
    toCbor _ = node "IfcPragma_Unknown" []

instance ToCbor Type where
    toCbor (TVar (TyVar name num _)) = node "Type_Var" [ toCbor name, toCbor num ]
    toCbor (TCon (TyCon name _ sort)) = node "Type_Con" [ toCbor name, toCbor sort ]
    toCbor (TCon (TyNum val _)) = node "Type_Num" [ toCbor val ]
    toCbor (TAp ty1 ty2) = node "Type_Ap" [ toCbor ty1, toCbor ty2 ]
    toCbor (TGen _ _) = node "Type_Gen" [ ]
    toCbor (TDefMonad _) = node "Type_DefMonad" [ ]

instance ToCbor TyVar where
    toCbor (TyVar name _num _kind) = toCbor name

instance ToCbor CQType where
    toCbor (CQType _ ty) = toCbor ty

instance ToCbor TISort where
    toCbor (TItype _ ty) = node "TySort_Type" [ toCbor ty ]
    toCbor (TIdata is) = node "TySort_Data" [ toCbor is ]
    toCbor (TIstruct sst is) = node "TySort_Struct" [ toCbor sst, toCbor is ]
    toCbor TIabstract = node "TySort_Abstract" [ ]

instance ToCbor StructSubType where
    toCbor SStruct = node "StructKind_Struct" [ ]
    toCbor SClass = node "StructKind_Class" [ ]
    toCbor (SDataCon _ _) = node "StructKind_Data" [ ]
    toCbor (SInterface _) = node "StructKind_Ifc" [ ]

instance ToCbor Id where
    toCbor i = node "Id"
        [ toCbor $ getIdPosition i
        , toCbor $ T.pack $ getIdString i
        , toCbor $ T.pack $ getIdQualString i
        ]

instance ToCbor IdK where
    toCbor (IdK i) = toCbor i
    toCbor (IdKind i _) = toCbor i
    toCbor (IdPKind i _) = toCbor i

instance ToCbor Position where
    toCbor (Position file line col isStdlib) = node "Position"
        [ toCbor file, toCbor line, toCbor col, toCbor isStdlib ]

instance ToCbor FS.FString where
    toCbor fs = CBOR.TString $ T.pack $ FS.getFString fs


cPackageToCbor :: CPackage -> CBOR.Term
cPackageToCbor = toCbor

cPackageToCborBytes :: CPackage -> BS.ByteString
cPackageToCborBytes p = CBOR.toStrictByteString $ CBOR.encodeTerm $ toCbor p
