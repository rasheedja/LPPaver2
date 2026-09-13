import { exprHashToExpr, type Expr, type ExprDict, type ExprHash } from './exprs';

export type Form = {
  f: FormF<Expr, Form>;
  hash: FormHash;
};

export type FormF<E, F> =
  | FormComp<E>
  | FormUnary<F>
  | FormBinary<F>
  | FormIfThenElse<F>
  | FormTrue
  | FormFalse;

export type FormComp<E> = {
  tag: 'FormComp';
  comp: BinaryComp;
  e1: E;
  e2: E;
};

export type FormUnary<F> = {
  tag: 'FormUnary';
  uconn: UnaryConn;
  f1: F;
};

export type FormBinary<F> = {
  tag: 'FormBinary';
  bconn: BinaryConn;
  f1: F;
  f2: F;
};

export type FormIfThenElse<F> = {
  tag: 'FormIfThenElse';
  fc: F;
  ft: F;
  ff: F;
};

export type FormTrue = {
  tag: 'FormTrue';
};

export type FormFalse = {
  tag: 'FormFalse';
};

export type BinaryComp = 'CompLe' | 'CompLeq' | 'CompEq' | 'CompNeq';
export type UnaryConn = 'ConnNeg';
export type BinaryConn = 'ConnAnd' | 'ConnOr' | 'ConnImpl';

export const binaryCompSymbolMap: Record<BinaryComp, string> = {
  CompLe: '<',
  CompLeq: '≤',
  CompEq: '=',
  CompNeq: '≠',
};

export const unaryConnSymbolMap: Record<UnaryConn, string> = {
  ConnNeg: '¬',
};

export const binaryConnSymbolMap: Record<BinaryConn, string> = {
  ConnAnd: '∧',
  ConnOr: '∨',
  ConnImpl: '→',
};

export type Comparison = {
  comp: BinaryComp;
  e1: Expr;
  e2: Expr;
};

export function decomposeBinaryComp(
  form: Form,
): Comparison | { comp?: never; e1?: never; e2?: never } {
  if (form.f.tag === 'FormComp') {
    return form.f;
  }
  return {};
}

export type FormHash = string;
export type FormDict = Record<FormHash, FormF<ExprHash, FormHash>>;

export type FormOrExprHash =
  | { type: 'form'; formHash: FormHash }
  | { type: 'expr'; exprHash: ExprHash };

export function formHashToForm(
  formHash: FormHash,
  dictF: FormDict,
  dictE: ExprDict,
): Form | undefined {
  const formF = dictF[formHash];
  if (!formF) {
    return undefined;
  }

  switch (formF.tag) {
    case 'FormComp':
      return {
        f: {
          tag: 'FormComp',
          comp: formF.comp,
          e1: exprHashToExpr(formF.e1, dictE),
          e2: exprHashToExpr(formF.e2, dictE),
        },
        hash: formHash,
      };
    case 'FormUnary':
      const f = formHashToForm(formF.f1, dictF, dictE);
      if (!f) return undefined;
      return {
        f: {
          tag: 'FormUnary',
          uconn: formF.uconn,
          f1: f,
        },
        hash: formHash,
      };
    case 'FormBinary':
      const f1 = formHashToForm(formF.f1, dictF, dictE);
      const f2 = formHashToForm(formF.f2, dictF, dictE);
      if (!f1 || !f2) return undefined;
      return {
        f: {
          tag: 'FormBinary',
          bconn: formF.bconn,
          f1: f1,
          f2: f2,
        },
        hash: formHash,
      };
    case 'FormIfThenElse':
      const fc = formHashToForm(formF.fc, dictF, dictE);
      const ft = formHashToForm(formF.ft, dictF, dictE);
      const ff = formHashToForm(formF.ff, dictF, dictE);
      if (!fc || !ft || !ff) return undefined;
      return {
        f: {
          tag: 'FormIfThenElse',
          fc: fc,
          ft: ft,
          ff: ff,
        },
        hash: formHash,
      };
    case 'FormTrue':
      return { f: { tag: 'FormTrue' }, hash: formHash };
    case 'FormFalse':
      return { f: { tag: 'FormFalse' }, hash: formHash };
    default:
      throw new Error(`Unknown form tag: ${(formF as any).tag}`);
  }
}
