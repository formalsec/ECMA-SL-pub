module.exports = {
  hasStrictDirective
}

function hasStrictDirective(stmts) {
  /* 14.1 Directive Prologues and the Use Strict Directive */

  /* A Directive Prologue is the longest sequence of ExpressionStatement productions occurring as the initial
    SourceElement productions of a Program or FunctionBody and where each ExpressionStatement in the sequence
    consists entirely of a StringLiteral token followed a semicolon. The semicolon may appear explicitly or may be
    inserted by automatic semicolon insertion. A Directive Prologue may be an empty sequence. */

  /* A Use Strict Directive is an ExpressionStatement in a Directive Prologue whose StringLiteral is either the exact
    character sequences "use strict" or 'use strict'. A Use Strict Directive may not contain an
    EscapeSequence or LineContinuation. */

  /* A Directive Prologue may contain more than one Use Strict Directive. However, an implementation may issue
    a warning if this occurs. */

  /* NOTE The ExpressionStatement productions of a Directive Prologue are evaluated normally during evaluation of the
    containing SourceElements production. Implementations may define implementation specific meanings for
    ExpressionStatement productions which are not a Use Strict Directive and which occur in a Directive Prologue. If an
    appropriate notification mechanism exists, an implementation should issue a warning if it encounters in a Directive
    Prologue an ExpressionStatement that is not a Use Strict Directive or which does not have a meaning defined by the
    implementation. */
  if (!stmts) return false;

  if (stmts.length > 0) {
    let useStrictDirectiveFound = false;

    for(let i = 0; i < stmts.length; i++) {
      const stmt = stmts[i];

      if (stmt.hasOwnProperty("directive")) {
        /* This "directive" property is present while the statement is an ExpressionStatement, it consists
          entirely of a StringLiteral and it occurs in the sequence of the initial SourceElement productions
          of a Program or FunctionBody. */
        /* In the case of having multiple "directives", "use strict" can be any of them and not necessarily
           in the first ExpressionStatement. */
        if (stmt.directive === "use strict") {
          useStrictDirectiveFound = true;
          break
        }
      } else {
        /* In this case, we already know that the statement is either not an ExpressionStatement or it does not consist
           entirely of a StringLiteral */
        break
      }
    }

    return useStrictDirectiveFound;
  }

  return false
}
