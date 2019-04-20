import { Injectable } from '../../core/src/core'; //'mojiito-core';
import { Expression, ExpressionParser } from './expression/expression';
import { isPresent } from '../../facade/src/lang';

// tslint:disable-next-line:max-line-length
const BIND_NAME_REGEXP = /^(?:(?:(?:(bind-)|(let-)|(ref-|#)|(on-)|(bindon-)|(@))(.+))|\[\(([^\)]+)\)\]|\[([^\]]+)\]|\(([^\)]+)\))$/;

// Group 1 = "bind-"
const KW_BIND_IDX = 1;
// Group 2 = "let-"
const KW_LET_IDX = 2;
// Group 3 = "ref-/#"
const KW_REF_IDX = 3;
// Group 4 = "on-"
const KW_ON_IDX = 4;
// Group 5 = "bindon-"
const KW_BINDON_IDX = 5;
// Group 6 = "@"
const KW_AT_IDX = 6;
// Group 7 = the identifier after "bind-", "let-", "ref-/#", "on-", "bindon-" or "@"
const IDENT_KW_IDX = 7;
// Group 8 = identifier inside [()]
const IDENT_BANANA_BOX_IDX = 8;
// Group 9 = identifier inside []
const IDENT_PROPERTY_IDX = 9;
// Group 10 = identifier inside ()
const IDENT_EVENT_IDX = 10;

const CLASS_ATTR = 'class';

@Injectable()
export class BindingParser {

  constructor(private _exprParser: ExpressionParser) { }

  parse(name: string, expression: string) {
    const bindParts = name.match(BIND_NAME_REGEXP);
    const expr = this._exprParser.parse(expression);
    let key: string;

    if (isPresent(bindParts[KW_BIND_IDX])) {
      unsupported('Property binding', name, expression);
    } else if (bindParts[KW_REF_IDX]) {
      unsupported('Variable declaration', name, expression);
    } else if (bindParts[KW_ON_IDX]) {
      return new EventBindingParseResult(bindParts[IDENT_KW_IDX], expr);
    } else if (bindParts[KW_BINDON_IDX]) {
      unsupported('Two way binding', name, expression);
    } else if (bindParts[KW_AT_IDX]) {
      unsupported('Animations', name, expression);
    } else if (bindParts[IDENT_BANANA_BOX_IDX]) {
      unsupported('Two way binding', name, expression);
    } else if (bindParts[IDENT_PROPERTY_IDX]) {
      unsupported('Property binding', name, expression);
    } else if (bindParts[IDENT_EVENT_IDX]) {
      return new EventBindingParseResult(bindParts[IDENT_EVENT_IDX], expr);
    } else {
      throw new Error(`Unknown binding name: ${name}`);
    }
  }
}

export class BindingParseResult {
  constructor(public expression: Expression) { }
}

export class EventBindingParseResult extends BindingParseResult {
  constructor(public eventName: string, expression: Expression) {
    super(expression);
  }
}

function unsupported(type: string, name: string, expression: string) {
  throw new Error(`${type} is not yet supported: ${name}="${expression}"`);
}
