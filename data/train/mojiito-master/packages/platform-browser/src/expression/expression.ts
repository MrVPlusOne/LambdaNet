import { Token, ExpressionParser } from './expression_parser';

export { Token, ExpressionParser };

export class Expression {
  constructor(public tokens: Token[]) { }
}
